#lang racket

(require
  syntax/id-table
  "scope.rkt")

(provide (all-defined-out))

; A node has a reference to its original syntax object.
(struct node (stx) #:transparent)

; A module is a node that contains design units.
(struct module node (body))

; A named element is a node with a name attribute.
(struct named-elt node (name))

; A design unit is a named element with a list of I/O declarations.
(struct design-unit named-elt (params body local-scope))

(struct interface design-unit ())

(struct component design-unit ())

(define (make-local-scope lst [sc (make-immutable-free-id-table)])
  (for/fold ([acc sc])
            ([i (in-list lst)] #:when (named-elt? i))
    (dict-set acc (named-elt-name i) i)))

(define (make-design-unit ctor stx name params body)
  (ctor stx name params body
        (make-local-scope params (make-local-scope body))))

(define (design-unit-lookin unit name)
  (dict-ref (design-unit-local-scope unit) name
    (λ () (raise-syntax-error #f "No element with this name" name))))

; A data port is a named element with a mode (input, output) and a data type.
(struct data-port named-elt (mode type))

; A composite port is a named element with a mode (use, flip),
; an interface type and interface arguments.
(struct composite-port named-elt (mode mult intf-name args))

; An inline composite port has no name and no multiplicity.
(struct inline-composite-port node (mode intf-name args))

(struct parameter named-elt (type))

(struct constant named-elt (type expr))

(struct assignment node (target expr))

(struct literal-expr node (value))

(struct name-expr node (name))

(struct field-expr node (expr field-name type-name))

(struct indexed-expr node (expr indices))

(struct call-expr node (fn-name args))

(define (resolve n)
  (match n
    [(name-expr _ name)
     (lookup name)]

    [(field-expr _ expr field-name _)
     (match (resolve expr)
       [(composite-port _ _ _ _ intf-name _)
        (design-unit-lookin (lookup intf-name interface?) field-name)]
       [_ (raise-syntax-error #f "Expression not suitable for field access" (node-stx n))])]

    [(indexed-expr _ expr _)
     (resolve expr)]

    [_ #f]))

; A signal expression wraps an expression whose result is a signal
; accessed for reading.
(struct signal-expr node (expr))

; A lift expression converts an expression that operates on values
; into an expression that operates on signals.
; The bindings are a dictionary that associates names to
; signal expressions used in expr.
(struct lift-expr node (bindings expr))
