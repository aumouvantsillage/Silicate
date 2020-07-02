#lang racket

(provide (all-defined-out))

; A node has a reference to its original syntax object.
(struct node (stx))

; A module is a node that contains design units.
(struct module node (body))

; A named element is a node with a name attribute.
(struct named-elt node (name))

; An interface is a named element with a list of I/O declarations.
(struct interface named-elt (io))

; A component combines in interface with a list of statements.
(struct component interface (body))

; A data port is a named element with a mode (input, output) and a data type.
(struct data-port named-elt (mode type))

; A composite port is a named element with a mode (use, flip),
; an interface type and interface arguments.
(struct composite-port named-elt (mode mult type args))

(struct inline-composite-port node (mode type args))

(struct parameter named-elt (type))

(struct constant named-elt (type expr))

(struct assignment node (target expr))

(struct name-expr node (name))

(struct indexed-expr node (expr indices))

(struct literal-expr node (value))
