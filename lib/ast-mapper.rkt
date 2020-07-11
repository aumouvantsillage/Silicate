#lang racket

(require
  "expander.rkt"
  (for-syntax
    racket
    syntax/parse
    (prefix-in stx- "syntax-classes.rkt")
    (prefix-in ast- "ast.rkt")
    "scope.rkt"))

(provide
  (for-syntax
      syntax->ast
      ast->syntax
      ast->proc))

(begin-for-syntax
  (define (bind-named-elt! elt)
    (bind! (ast-named-elt-name elt) elt))

  (define (syntax->ast stx)
    (syntax-parse stx
      #:datum-literals [begin-silicate]
      [(begin-silicate mod)
       (syntax->ast #'mod)]

      [:stx-module
       (with-scope
         (ast-module stx (map syntax->ast (attribute body))))]

      [:stx-interface
       (bind-named-elt!
         (with-scope
           (ast-make-design-unit ast-interface stx #'name
             (map syntax->ast (attribute param))
             (map syntax->ast (attribute body)))))]

      [:stx-component
       (bind-named-elt!
         (with-scope
           (ast-make-design-unit ast-component stx #'name
             (map syntax->ast (attribute param))
             (map syntax->ast (attribute body)))))]

      [:stx-parameter
       (bind-named-elt!
         (ast-parameter stx #'name (syntax->ast #'type)))]

      [:stx-data-port
       (bind-named-elt!
         (ast-data-port stx #'name (syntax->datum #'mode) (syntax->ast #'type)))]

      [:stx-composite-port
       (define m (attribute mult))
       (bind-named-elt!
         (ast-composite-port stx #'name
           (syntax->datum #'mode)
           (if m (syntax->ast m) (ast-literal-expr stx 1))
           (add-scope #'type)
           (map syntax->ast (attribute arg))))]

      [:stx-inline-composite-port
       (ast-inline-composite-port stx (syntax->datum #'mode)
         (add-scope #'type) (map syntax->ast (attribute arg)))]

      [:stx-constant
       (bind-named-elt!
         (ast-constant stx #'name (syntax->ast #'type) (syntax->ast #'expr)))]

      [:stx-assignment
       (ast-assignment stx (syntax->ast #'target) (syntax->ast #'expr))]

      [:stx-name-expr
       (ast-name-expr stx (add-scope #'name))]

      [:stx-field-expr
       (ast-field-expr stx (syntax->ast #'expr) #'name #f)]

      [:stx-indexed-expr
       (ast-indexed-expr stx (syntax->ast #'expr) (map syntax->ast (attribute index)))]

      [:stx-literal-expr
       (ast-literal-expr stx (syntax->datum #'value))]))

  (define (ast->syntax n)
    (match n
      [(ast-module stx body)
       (quasisyntax/loc stx
         (module #,@(map ast->syntax body)))]

      [(ast-component stx name params body _)
       (quasisyntax/loc stx
         (component #,name
           #,@(map ast->syntax params)
           #,@(map ast->syntax body)))]

      [(ast-interface stx name params body _)
       (quasisyntax/loc stx
         (interface #,name
           #,@(map ast->syntax params)
           #,@(map ast->syntax body)))]

      [(ast-data-port stx name mode type)
       (quasisyntax/loc stx
         (data-port #,name #,mode #,(ast->syntax type)))]

      [(ast-composite-port stx name mode mult intf-name args)
       (quasisyntax/loc stx
         (composite-port #,name (multiplicity #,(ast->syntax mult)) #,mode #,intf-name #,@(map ast->syntax args)))]

      [(ast-parameter stx name type)
       (quasisyntax/loc stx
         (parameter #,name #,(ast->syntax type)))]

      [(ast-constant stx name type expr)
       (quasisyntax/loc stx
         (constant #,name #,(ast->syntax type) #,(ast->syntax expr)))]

      [(ast-assignment stx target expr)
       (quasisyntax/loc stx
         (assignment #,(ast->syntax target) #,(ast->syntax expr)))]

      [(ast-name-expr stx name)
       (quasisyntax/loc stx
         (name-expr #,name))]

      [(ast-field-expr stx expr field-name type-name)
       (quasisyntax/loc stx
         (field-expr #,(ast->syntax expr) #,field-name #,type-name))]

      [(ast-indexed-expr stx expr indices)
       (quasisyntax/loc stx
         (indexed-expr #,(ast->syntax expr) #,@(map ast->syntax indices)))]

      [(ast-literal-expr stx value)
       (quasisyntax/loc stx
         (literal-expr #,value))]

      [(ast-signal-expr stx expr)
       (quasisyntax/loc stx
         (signal-expr #,(ast->syntax expr)))]

      [(ast-lift-expr stx bindings expr)
       (quasisyntax/loc stx
         (lift-expr #,@(for/list ([(k v) (in-dict bindings)])
                         (list k (ast->syntax v)))
                    #,(ast->syntax expr)))]

      ; Common case: return the syntax object associated with this ast node
      [(ast-node stx) stx]

      ; If n is not an AST node, return it directly.
      [_ n]))

  ; TODO
  (define (ast->proc n)
    #'(define (#%silicate-make-ast)
        (void))))
