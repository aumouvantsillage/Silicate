#lang racket

(require
  "expander.rkt"
  (for-syntax
    racket
    syntax/parse
    syntax/parse/define
    (prefix-in stx/ "syntax-classes.rkt")
    (prefix-in ast/ "ast.rkt")
    "scope.rkt"))

(provide
  (for-syntax
      syntax->ast
      ast->syntax
      ast->proc))

(begin-for-syntax
  (define (bind-named-elt! elt)
    (bind! (ast/named-elt-name elt) elt))

  (define-simple-macro (attr->ast name)
    (syntax->ast (attribute name)))

  (define-simple-macro (attr->datum name)
    (syntax->datum (attribute name)))

  (define-simple-macro (attr? name)
    (if (attribute name) #t #f))

  (define (syntax->ast stx)
    (syntax-parse stx
      #:datum-literals [begin-silicate]
      [(begin-silicate mod)
       (attr->ast mod)]

      [:stx/module
       (with-scope
         (ast/module stx (attr->ast body)))]

      [:stx/interface
       (bind-named-elt!
         (with-scope
           (ast/make-design-unit ast/interface stx #'name (attr->ast param) (attr->ast body))))]

      [:stx/component
       (bind-named-elt!
         (with-scope
           (ast/make-design-unit ast/component stx #'name (attr->ast param) (attr->ast body))))]

      [:stx/parameter
       (bind-named-elt!
         (ast/parameter stx #'name (attr->ast type)))]

      [:stx/data-port
       (bind-named-elt!
         (ast/data-port stx #'name (attr->datum mode) (attr->ast type)))]

      [:stx/composite-port
       ; Provide a default value for the mult attribute.
       ; For some reason, it cannot be set as a #:defaults the syntax class.
       (define mult^ (if (attribute mult)
                       (attr->ast mult)
                       (ast/literal-expr stx 1)))
       (bind-named-elt!
         (ast/composite-port stx #'name (attr? flip?) mult^ (add-scope #'intf-name) (attr->ast arg)))]

      [:stx/inline-composite-port
       (ast/inline-composite-port stx (attr? flip?) (add-scope #'intf-name) (attr->ast arg))]

      [:stx/constant
       (bind-named-elt!
         (ast/constant stx #'name (attr->ast expr)))]

      [:stx/local-signal
       (bind-named-elt!
         (ast/local-signal stx #'name (attr->ast expr)))]

      [:stx/assignment
       (ast/assignment stx (attr->ast target) (attr->ast expr))]

      [:stx/literal-expr
       (ast/literal-expr stx (attr->datum value))]

      [:stx/name-expr
       (ast/name-expr stx (add-scope #'name))]

      [:stx/field-expr
       (ast/field-expr stx (attr->ast expr) #'field-name #f)]

      [:stx/indexed-expr
       (ast/indexed-expr stx (attr->ast expr) (attr->ast index))]

      [:stx/call-expr
       (ast/call-expr stx #'fn-name (attr->ast arg))]

      [(child ...)
       (map syntax->ast (attribute child))]))

  (define (ast->syntax n)
    (match n
      [(ast/module stx body)
       (quasisyntax/loc stx
         (module #,@(ast->syntax body)))]

      [(ast/component stx name params body _)
       (quasisyntax/loc stx
         (component #,name #,@(ast->syntax params) #,@(ast->syntax body)))]

      [(ast/interface stx name params body _)
       (quasisyntax/loc stx
         (interface #,name #,@(ast->syntax params) #,@(ast->syntax body)))]

      [(ast/data-port stx name mode type)
       (quasisyntax/loc stx
         (data-port #,name #,mode #,(ast->syntax type)))]

      [(ast/composite-port stx name flip? mult intf-name args)
       (quasisyntax/loc stx
         (composite-port #,name
           (multiplicity #,(ast->syntax mult))
           #,@(if flip? (list #'flip) empty)
           #,intf-name #,@(ast->syntax args)))]

      [(ast/parameter stx name type)
       (quasisyntax/loc stx
         (parameter #,name #,(ast->syntax type)))]

      [(ast/constant stx name expr)
       (quasisyntax/loc stx
         (constant #,name #,(ast->syntax expr)))]

      [(ast/local-signal stx name expr)
       (quasisyntax/loc stx
         (local-signal #,name #,(ast->syntax expr)))]

      [(ast/assignment stx target expr)
       (quasisyntax/loc stx
         (assignment #,(ast->syntax target) #,(ast->syntax expr)))]

      [(ast/literal-expr stx value)
       (quasisyntax/loc stx
         (literal-expr #,value))]

      [(ast/name-expr stx name)
       (quasisyntax/loc stx
         (name-expr #,name))]

      [(ast/field-expr stx expr field-name type-name)
       (quasisyntax/loc stx
         (field-expr #,(ast->syntax expr) #,field-name #,type-name))]

      [(ast/indexed-expr stx expr indices)
       (quasisyntax/loc stx
         (indexed-expr #,(ast->syntax expr) #,@(ast->syntax indices)))]

      [(ast/call-expr stx fn-name args)
       (quasisyntax/loc stx
         (call-expr #,fn-name #,@(ast->syntax args)))]

      [(ast/signal-expr stx expr)
       (quasisyntax/loc stx
         (signal-expr #,(ast->syntax expr)))]

      [(ast/static-expr stx expr)
       (quasisyntax/loc stx
         (static-expr #,(ast->syntax expr)))]

      [(ast/lift-expr stx bindings expr)
       (quasisyntax/loc stx
         (lift-expr #,@(for/list ([(k v) (in-dict bindings)])
                         (list k (ast->syntax v)))
                    #,(ast->syntax expr)))]

      ; Common case: return the syntax object associated with this ast node
      [(ast/node stx) stx]

      ; Convert a list of AST nodes into a list of syntax objects.
      [(? list?)
       (map ast->syntax n)]

      ; If n is not an AST node, return it directly.
      [_ n]))

  ; TODO
  (define (ast->proc n)
    #'(define (#%silicate-make-ast)
        (void))))
