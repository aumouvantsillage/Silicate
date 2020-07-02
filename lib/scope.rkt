#lang racket

(require syntax/id-table)

(provide
  with-scope
  add-scope
  bind!
  lookup)

; A scope has an optional parent scope
; and a table that maps ids to syntax elements.
(struct scope (parent table))

(define current-scope (make-parameter #f))

(define-syntax-rule (with-scope body ...)
  (parameterize ([current-scope (make-scope)])
    body ...))

; Create a new scope with the given parent and an empty table.
(define (make-scope [parent (current-scope)])
  (scope parent (make-free-id-table)))

; Add scope information to a syntax object recursively.
(define (add-scope stx [sc (current-scope)])
  (syntax-property stx 'parent-scope sc))

; Add the given element to its parent scope with the given name.
(define (bind! name elt [sc (current-scope)])
  (dict-set! (scope-table sc) name elt)
  elt)

; Lookup a name in a scope chain. If no scope is specified,
; start at the scope attached to the name.
; Returns the corresponding AST node, or raise an error if not found.
(define (lookup name [pred (λ (x) #t)] [sc (syntax-property name 'parent-scope)])
  (unless sc
    (raise-syntax-error #f "Unbound identifier" name))
  (define res (dict-ref (scope-table sc) name
                (λ () (lookup name pred (scope-parent sc)))))
  (unless (pred res)
    (raise-syntax-error #f "Invalid target" name))
  res)
