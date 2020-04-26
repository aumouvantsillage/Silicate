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
  (writeln intf-id)
  #`(struct #,intf-id
      #,(for/list ([f fields])
          (syntax-case f ()
            ; Keep only the name of each field.
            [(_ field-id _) #'field-id]))))

(define-for-syntax (interface-to-constructor intf-id fields)
  (with-syntax ([intf-ctor-id (format-ctor-id intf-id)])
    #`(define (intf-ctor-id)
        ; Call the default constructor and initialize each field.
        (#,intf-id #,@(for/list ([f fields])
                        ; Call a constructor for each field.
                        (syntax-case f ()
                          ; If the field is a plain input or output,
                          ; create an empty box.
                          [(in  field-id _) #'(box #f)]
                          [(out field-id _) #'(box #f)]
                          ; If the field uses an interface,
                          ; call the constructor for the target interface.
                          [(_ field-id field-intf-id)
                           (with-syntax ([field-intf-ctor-id (format-ctor-id #'field-intf-id)])
                             #'(field-intf-ctor-id))]))))))

; Syntax of interfaces:
;
; intf ::= (interface intf-id (mode field-id type expr?) ...)
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

; Get a proxy to a signal from an interface of the current component.
; Transforms: (interface-ref a b c d)
; Into:       (signal-proxy (unbox (d (c (b a)))))
(define-syntax interface-ref
  (syntax-rules ()
    [(interface-ref x)
     (signal-proxy (unbox x))]
    [(interface-ref a b c ...)
     (let* ([a* a]
            [b* b]
            [ba (if (vector? a*) (vector-ref a* b*) (b* a*))])
       (interface-ref ba c ...))]))

; Assign a signal to a field in the interface of the current component.
; Transforms: (interface-set! a b c d y)
; Into:       (set-box! (d (c (b a)))) y)
(define-syntax interface-set!
  (syntax-rules ()
    [(interface-set! x y)
     (set-box! x y)]
    [(interface-set! a b c ... y)
     (interface-set! (b a) c ... y)]))
