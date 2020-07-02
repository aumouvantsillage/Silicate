#lang racket

(require
  syntax/parse
  (prefix-in ast: "ast.rkt")
  "scope.rkt")

(provide syntax->ast)

(define (bind-named-elt! elt)
  (bind! (ast:named-elt-name elt) elt))

(define (syntax->ast stx)
  (syntax-parse stx
    #:datum-literals [begin-silicate interface component
                      data-port composite-port inline-composite-port
                      multiplicity in out use flip
                      assign
                      name-exp index-exp]
    [(begin-silicate body ...)
     (with-scope
       (ast:module stx (map syntax->ast (attribute body))))]

    [(interface name (io ...))
     (bind-named-elt!
       (with-scope
         (ast:interface stx #'name (map syntax->ast (attribute io)))))]

    [(component name (io ...) body ...)
     (bind-named-elt!
       (with-scope
         (ast:component stx #'name (map syntax->ast (attribute io))
           (with-scope
             (map syntax->ast (attribute body))))))]

    [(data-port name mode type)
     (bind-named-elt!
       (ast:data-port stx #'name (syntax->datum #'mode) (syntax->ast #'type)))]

    [(composite-port name (~optional (multiplicity mult)) mode type arg ...)
     (define m (attribute mult))
     (bind-named-elt!
       (ast:composite-port stx #'name (syntax->datum #'mode)
         (and m (syntax->ast m)) (syntax->ast #'type)
         (map syntax->ast (attribute arg))))]

    [(inline-composite-port mode type arg ...)
     (ast:inline-composite-port stx (syntax->datum #'mode)
       (syntax->ast #'type) (map syntax->ast (attribute arg)))]

    [(parameter name type)
     (bind-named-elt!
       (ast:parameter #'name (syntax->ast #'type)))]

    [(constant name type expr)
     (bind-named-elt!
       (ast:constant #'name (syntax->ast #'type) (syntax->ast #'expr)))]

    [(assignment target expr)
     (ast:assignment (syntax->ast #'target) (syntax->ast #'expr))]

    [(name-expr name)
     (ast:name-expr (add-scope #'name))]

    [(indexed-expr expr index ...)
     (ast:indexed-expr (syntax->ast #'expr) (map syntax->ast (attribute index)))]

    [(literal-expr value)
     (ast:literal-expr (syntax->datum #'value))]))
