#lang racket

(require syntax/id-table)

(provide (all-defined-out))

; A node has a reference to its original syntax object.
(struct node (stx))

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

(struct name-expr node (name))

(struct field-expr node (expr field-name type-name))

(struct indexed-expr node (expr indices))

(struct literal-expr node (value))

; A signal expression wraps an expression whose result is a signal
; accessed for reading.
(struct signal-expr node (expr))

; A lift expression converts an expression that operates on values
; into an expression that operates on signals.
; The bindings are a dictionary that associates names to
; signal expressions used in expr.
(struct lift-expr node (bindings expr))
