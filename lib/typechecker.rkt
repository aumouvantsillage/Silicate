#lang racket

(require
  "expander.rkt"
  (for-syntax
    racket
    syntax/stx
    syntax/parse
    (prefix-in stx/ "syntax-classes.rkt")
    (prefix-in meta/ "metadata.rkt")
    "scope.rkt"))

(provide (for-syntax typecheck))

(begin-for-syntax
  (define (typecheck* stx)
    (syntax-parse stx
      [(item ...) (quasisyntax/loc stx #,(map typecheck (attribute item)))]
      [_          stx]))

  (define (typecheck stx)
    (with-lookup-cache
      ; TODO inline composite ports
      (syntax-parse stx
        [:stx/constant     (typecheck-constant stx #'name (typecheck #'expr))]
        [:stx/local-signal (typecheck-local-signal stx #'name (typecheck #'expr))]
        [:stx/assignment   (typecheck-assignment stx (typecheck #'target) (typecheck #'expr))]
        [:stx/field-expr   (typecheck-field-expr stx (typecheck #'expr) #'field-name)]
        [:stx/indexed-expr (typecheck-indexed-expr stx (typecheck #'expr) (typecheck* #'(index ...)))]
        [:stx/call-expr    (typecheck-call-expr stx #'fn-name (typecheck* #'(arg ...)))]
        [_                 (typecheck* stx)])))

  ; Find the metadata of the given expression result.
  (define (resolve stx)
    (syntax-parse stx
      [:stx/name-expr
       ; For a name expression, lookup the metadata in the name's scope.
       (lookup #'name)]

      [:stx/field-expr
       ; For a field expression, resolve the left-hand side first.
       (match (resolve #'expr)
         [(meta/composite-port s)
          ; If the lhs maps to a composite port, look up the given field name
          ; in the target interface.
          (define/syntax-parse :stx/composite-port s)
          (meta/design-unit-ref (lookup #'intf-name meta/interface?) #'field-name)]

         [(meta/instance s)
          ; If the lhs maps to an instance, look up the given field name
          ; in the target component.
          (define/syntax-parse :stx/instance s)
          (meta/design-unit-ref (lookup #'comp-name meta/component?) #'field-name)]

         ; TODO resolve field access for structured data types

         [_ (raise-syntax-error #f "Expression not suitable for field access" stx)])]
      [:stx/indexed-expr (resolve #'expr)]
      [_ stx]))

  (define (typecheck-constant stx name expr)
    (unless (static-value? expr)
      (raise-syntax-error #f "Non-static value cannot be assigned to constant" expr))
    ; TODO check expression type
    (quasisyntax/loc stx
      (constant #,name #,expr)))

  (define (static-value? stx)
    (syntax-parse stx
      [:stx/literal-expr #t]
      [:stx/name-expr    (meta/constant? (lookup #'name))]
      [:stx/field-expr   (static-value? #'expr)]
      [:stx/indexed-expr (and (static-value? #'expr) (andmap static-value? (attribute index)))]
      [:stx/call-expr    (andmap static-value? (attribute arg))]
      [_                 #f]))

  (define (typecheck-local-signal stx name expr)
    ; TODO check expression type
    (quasisyntax/loc stx
      (local-signal #,name #,(typecheck-assigned-expr expr))))

  (define (typecheck-assignment stx target expr)
    ; TODO check port mode: out flip/in inst/in inst/flip/out
    ; TODO support assignment from composite to composite.
    ; TODO check circular dependencies.
    (quasisyntax/loc stx
      (assignment #,target #,(typecheck-assigned-expr expr))))

  (define (typecheck-assigned-expr stx)
    (syntax-parse (lift-if-needed stx)
      [:stx/lift-expr
       ; If a lift-expr wraps a signal read, wrap it also in a signal-expr.
       (if (meta/signal? (resolve #'expr))
         (quasisyntax/loc stx (lift-expr binding ... (signal-expr expr)))
         this-syntax)]

      [_
       ; If stx has a static value, wrap it in a static-expr.
       ; If stx resolves to a signal, wrap it in a signal-expr.
       (cond [(static-value? stx)          (quasisyntax/loc stx (static-expr #,stx))]
             [(meta/signal? (resolve stx)) (quasisyntax/loc stx (signal-expr #,stx))]
             [else                         stx])]))

  (define (lift-if-needed stx)
    (syntax-parse stx
      [:stx/call-expr    (lift-if-needed* stx (attribute arg)
                                          (λ (lst) #`(call-expr fn-name #,@lst)))]
      [:stx/indexed-expr (lift-if-needed* stx (cons #'expr (attribute index))
                                          (λ (lst) #`(indexed-expr #,@lst)))]
      [:stx/field-expr   (lift-if-needed* stx (list #'expr)
                                          (λ (lst) #`(field-expr #,(first lst) field-name type-name)))]
      [_                 stx]))

  (define (lift-if-needed* stx lst thunk)
    (for/foldr ([b-lst empty] ; The list of bindings for the new lift-expr
                [a-lst empty] ; The list of arguments for the new call-expr
                ; If there are no bindings, return the original call-expr,
                ; else, make a new call-expr wrapped in a lift-expr.
                #:result (if (empty? b-lst)
                           stx
                           (quasisyntax/loc stx
                             (lift-expr #,@b-lst #,(thunk a-lst)))))
                ; Lift each argument if needed before proceeding.
               ([a (in-list (map lift-if-needed lst))])
      (syntax-parse a
        [:stx/lift-expr
         ; If the current argument is already lifted,
         ; accumulate its bindings and unwrap its expression.
         (values (append (attribute binding) b-lst)
                 (cons   #'expr              a-lst))]

        [_ #:when (meta/signal? (resolve a))
         ; If the argument resolves to a signal, wrap it in a signal-expr,
         ; create a binding and replace it with a name-expr.
         (define bname (gensym "lift"))
         (values (cons #`(#,bname (signal-expr #,a)) b-lst)
                 (cons #`(name-expr #,bname)         a-lst))]

        [_
         ; In the other cases, keep the current list of bindings
         ; and the current argument.
         (values b-lst (cons a a-lst))])))

  (define (typecheck-field-expr stx expr field-name)
    (match (resolve expr)
      [(meta/composite-port s)
       (define/syntax-parse :stx/composite-port s)
       ; Check that a port with that name exists in the interface.
       (meta/design-unit-ref (lookup #'intf-name meta/interface?) field-name)
       ; Return a new field-expr with an explicit interface name.
       (quasisyntax/loc stx
         (field-expr #,expr #,field-name intf-name))]

      [(meta/instance s)
       (define/syntax-parse :stx/instance s)
       ; Check that a port with that name exists in the component.
       (meta/design-unit-ref (lookup #'comp-name meta/component?) field-name)
       ; Return a new field-expr with an explicit component name.
       (quasisyntax/loc stx
         (field-expr #,expr #,field-name comp-name))]

      ; TODO support field access in structured types

      [_ (raise-syntax-error #f "Expression not suitable for field access" stx)]))

  (define (typecheck-indexed-expr stx expr indices)
    ; TODO support indexed access to array data types.
    (define r (resolve expr))
    (unless (or (meta/composite-port? r) (meta/instance? r))
      (raise-syntax-error #f "Expression not suitable for indexing" stx))
    (quasisyntax/loc stx
      (indexed-expr #,expr #,@indices)))

  (define (typecheck-call-expr stx fn-name args)
    ; TODO check that fn-name is bound or built-in
    ; TODO typecheck arguments against fn
    (quasisyntax/loc stx
      (call-expr #,fn-name #,@args))))
