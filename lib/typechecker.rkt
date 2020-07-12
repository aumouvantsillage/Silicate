#lang racket

(require
  (prefix-in ast- "ast.rkt")
  "scope.rkt")

(provide
  (contract-out
    [typecheck (-> ast-node? ast-node?)]))

(define (typecheck n)
  (match n
    [(ast-module stx body)
     (ast-module stx (map typecheck body))]

    [(ast-interface stx name params body _)
     (ast-make-design-unit ast-interface stx name
                           (map typecheck params)
                           (map typecheck body))]

    [(ast-component stx name params body _)
     (ast-make-design-unit ast-component stx name
                           (map typecheck params)
                           (map typecheck body))]

    [(ast-constant stx name type expr)
     (typecheck-constant stx name (typecheck type) (typecheck expr))]

    [(ast-assignment stx target expr)
     (typecheck-assignment stx (typecheck target) (typecheck expr))]

    [(ast-field-expr stx expr field-name _)
     (typecheck-field-expr stx (typecheck expr) (typecheck field-name))]

    [(ast-indexed-expr stx expr indices)
     (typecheck-indexed-expr stx (typecheck expr) (map typecheck indices))]

    [(ast-call-expr stx fn-name args)
     (typecheck-call-expr stx fn-name (map typecheck args))]

    [_ n]))

(define (typecheck-constant stx name type expr)
  (unless (ast-static-value? expr)
    (raise-syntax-error #f "Non-static value cannot be assigned to constant" expr))
  ; TODO check expression type
  (ast-constant stx name type expr))

(define (typecheck-assignment stx target expr)
  ; The target expression must refer to an output data port.
  ; TODO support other targets such as local signals.
  ; TODO check circular dependencies.
  (define p (ast-resolve target))
  (unless (and (ast-data-port? p) (eq? (ast-data-port-mode p) 'out))
    (raise-syntax-error #f "Invalid assignment target" target))

  ; If the source expression depends on signals, lift it.
  (define expr^ (lift-if-needed expr))

  (ast-assignment stx target
    ; TODO support local signals.
    (match expr^
      ; If expr^ is a lift-expr that wraps a signal read,
      ; wrap it also in a signal-expr.
      [(ast-lift-expr stx^ bindings body)
       (if (ast-data-port? (ast-resolve body))
         (ast-lift-expr stx^ bindings (ast-signal-expr stx^ body))
         expr^)]

      ; If expr^ has a static value, wrap it in a static-expr.
      [(? ast-static-value? (ast-node stx^))
       (ast-static-expr stx^ expr^)]

      ; If expr^ resolves to a signal, wrap it in a signal-expr.
      [(ast-node stx^) #:when (ast-data-port? (ast-resolve expr^))
       (ast-signal-expr stx^ expr^)]

      [_ expr^])))

(define (typecheck-field-expr stx expr name)
  (match (ast-resolve expr)
    [(ast-composite-port _ _ _ _ intf-name _)
     ; Check that a port with that name exists in the interface.
     (ast-design-unit-lookin (lookup intf-name ast-interface?) name)
     ; Return a new field-expr with an explicit interface name.
     (ast-field-expr stx expr name intf-name)]

    ; TODO support local signals.

    [_ (raise-syntax-error #f "Expression not suitable for field access" stx)]))

(define (typecheck-indexed-expr stx expr indices)
  (unless (ast-composite-port? (ast-resolve expr))
    (raise-syntax-error #f "Expression not suitable for indexing" stx))
  (ast-indexed-expr stx expr indices))

(define (typecheck-call-expr stx fn-name args)
  ; TODO check that fn-name is bound or built-in
  ; TODO typecheck arguments against fn
  (ast-call-expr stx fn-name args))

(define (lift-if-needed n)
  (match n
    [(ast-call-expr stx fn-name args)
     (lift-if-needed* n args (λ (lst) (ast-call-expr stx fn-name lst)))]

    [(ast-indexed-expr stx expr indices)
     (lift-if-needed* n (cons expr indices) (λ (lst) (ast-indexed-expr stx (first lst) (rest lst))))]

    [(ast-field-expr stx expr name type-name)
     (lift-if-needed* n (list expr) (λ (lst) (ast-field-expr stx (first lst) name type-name)))]

    [_ n]))

(define (lift-if-needed* n lst thunk)
  (for/foldr ([b-lst empty] ; The list of bindings for the new lift-expr
              [a-lst empty] ; The list of arguments for the new call-expr
              ; If there are no bindings, return the original call-expr,
              ; else, make a new call-expr wrapped in a lift-expr.
              #:result (if (empty? b-lst)
                         n
                         (ast-lift-expr (ast-node-stx n) b-lst (thunk a-lst))))
              ; Lift each argument if needed before proceeding.
             ([a (in-list (map lift-if-needed lst))])
    (match a
      ; If the current argument is already lifted,
      ; accumulate its bindings and unwrap its expression.
      [(ast-lift-expr _ bindings expr)
       (values (append bindings b-lst) (cons expr a-lst))]
      ; If the argument resolves to a signal,
      ; create a binding and replace it with a name-expr.
      ; TODO support local signals.
      [(ast-node stx) #:when (ast-data-port? (ast-resolve a))
       (define bname (gensym "lift"))
       (define new-binding (cons bname (ast-signal-expr stx a)))
       (define new-arg (ast-name-expr stx bname))
       (values (cons new-binding b-lst) (cons new-arg a-lst))]
      ; In the other cases, keep the current list of bindings
      ; and the current argument.
      [_ (values b-lst (cons a a-lst))])))
