#lang racket

(require (for-syntax racket/syntax))
(require "signal.rkt")

(provide
    define-interface
    interface-ref
    interface-set!)

(define-for-syntax (format-ctor-id intf-id)
  (format-id intf-id "make-~a" intf-id))

(define-for-syntax (interface-to-struct intf-id fields)
  #`(struct #,intf-id
      #,(for/list ([f fields])
          (syntax-case f ()
            ; Keep only the name of each field.
            [(_ field-id _ ...) #'field-id]))))

(define-for-syntax (interface-to-constructor intf-id fields)
  (with-syntax ([intf-ctor-id (format-ctor-id intf-id)])
    #`(define (intf-ctor-id)
        ; Call the default constructor and initialize each field.
        (#,intf-id #,@(for/list ([f fields])
                        (syntax-case f (in out inout use opposite)
                          ; If the field is a plain input or output,
                          ; create an empty box.
                          [(in     _ ...) #'(box #f)]
                          [(inout  _ ...) #'(box #f)]
                          [(out    _ ...) #'(box #f)]
                          ; If the field uses an interface,
                          ; call the constructor for the target interface.
                          [(_ field-id field-intf-id)
                           (with-syntax ([field-intf-ctor-id (format-ctor-id #'field-intf-id)])
                             #'(field-intf-ctor-id))]
                          ; If the field uses an interface and has a multiplicity,
                          ; create a vector with the result of the constructor for the target interface.
                          [(_ field-id field-intf-id expr)
                           (with-syntax ([field-intf-ctor-id (format-ctor-id #'field-intf-id)])
                             #'(if (> expr 1)
                                 (build-vector expr (λ (i) (field-intf-ctor-id)))
                                 (field-intf-ctor-id)))]))))))

; Syntax of interfaces:
;
; intf ::= (define-interface intf-id (mode field-id type expr?) ...)
; mode ::= in | out | use | opposite
; type ::= intf-id | data-type-spec
;
; TODO mode and data-type-spec are ignored
(define-syntax (define-interface stx)
  (syntax-case stx ()
    [(_ id field ...)
     (let ([fields (syntax->list #'(field ...))])
       #`(begin
           #,(interface-to-struct      #'id fields)
           #,(interface-to-constructor #'id fields)))]))

; Get the box that contains a signal from an interface of the current component.
(define-syntax interface-ref*
  (syntax-rules ()
    [(interface-ref* x) x]
    [(interface-ref* a b c ...)
     (let* ([va a]
            [vb b]
            [vba (if (vector? va) (vector-ref va vb) (vb va))])
       (interface-ref* vba c ...))]))

; Get a proxy to a signal from an interface of the current component.
(define-syntax interface-ref
  (syntax-rules ()
    [(interface-ref (s ...) x ...)
     ((lift (λ (s ...) (signal-first (interface-ref x ...)))) s ...)]
    [(interface-ref x ...)
     (signal-proxy (unbox (interface-ref* x ...)))]))

; Assign a signal to a field in the interface of the current component.
; Transforms: (interface-set! a b c d y)
; Into:       (set-box! (d (c (b a)))) y)
(define-syntax-rule (interface-set! x ... y)
  (set-box! (interface-ref* x ...) y))
