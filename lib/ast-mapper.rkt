#lang racket

(require
  syntax/parse
  "syntax-classes.rkt"
  (prefix-in ast: "ast.rkt")
  "scope.rkt")

(provide
  (contract-out
    [syntax->ast (-> syntax?   ast:node?)]
    [ast->syntax (-> ast:node? syntax?)]
    [ast->proc   (-> ast:node? syntax?)]))

(define (bind-named-elt! elt)
  (bind! (ast:named-elt-name elt) elt))

(define (syntax->ast stx)
  (syntax-parse stx
    #:datum-literals [begin-silicate]
    [(begin-silicate mod)
     (syntax->ast #'mod)]

    [:module
     (with-scope
       (ast:module stx (map syntax->ast (attribute body))))]

    [:interface
     (bind-named-elt!
       (with-scope
         (ast:interface stx #'name
           (map syntax->ast (attribute param))
           (map syntax->ast (attribute body)))))]

    [:component
     (bind-named-elt!
       (with-scope
         (ast:component stx #'name
           (map syntax->ast (attribute param))
           (map syntax->ast (attribute body)))))]

    [:parameter
     (bind-named-elt!
       (ast:parameter stx #'name (syntax->ast #'type)))]

    [:data-port
     (bind-named-elt!
       (ast:data-port stx #'name (syntax->datum #'mode) (syntax->ast #'type)))]

    [:composite-port
     (define m (attribute mult))
     (bind-named-elt!
       (ast:composite-port stx #'name (syntax->datum #'mode)
         (and m (syntax->ast m)) (add-scope #'type)
         (map syntax->ast (attribute arg))))]

    [:inline-composite-port
     (ast:inline-composite-port stx (syntax->datum #'mode)
       (add-scope #'type) (map syntax->ast (attribute arg)))]

    [:constant
     (bind-named-elt!
       (ast:constant stx #'name (syntax->ast #'type) (syntax->ast #'expr)))]

    [:assignment
     (ast:assignment stx (syntax->ast #'target) (syntax->ast #'expr))]

    [:name-expr
     (ast:name-expr stx (add-scope #'name))]

    [:field-expr
     (ast:field-expr stx (syntax->ast #'expr) #'name)]

    [:indexed-expr
     (ast:indexed-expr stx (syntax->ast #'expr) (map syntax->ast (attribute index)))]

    [:literal-expr
     (ast:literal-expr stx (syntax->datum #'value))]))

; TODO
(define (ast->syntax ast)
  #'(void))

; TODO
(define (ast->proc ast)
  #'(define (#%silicate-make-ast)
      (void)))
