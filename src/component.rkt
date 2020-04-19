#lang racket

(require (for-syntax racket/syntax))
(require "signal.rkt")

(provide
    interface
    interface-ref
    interface-set!)

(define-for-syntax (format-ctor-id intf-id)
  (format-id intf-id "make-~a" intf-id))

; Syntax of interfaces:
;
; intf ::= (interface intf-id ([mode field-id type] ...))
; mode ::= in | out | use | reverse
; type ::= intf-id | data-type-spec
;
; TODO mode and data-type-spec are ignored
(define-syntax (interface stx)
  (syntax-case stx ()
    [(_ intf-id (field ...))
     (with-syntax ([intf-ctor-id (format-ctor-id #'intf-id)])
       #`(begin
           ; Create a struct for this interface.
           (struct intf-id #,(for/list ([f (syntax->list #'(field ...))])
                               ; Keep only the name of each field.
                               (syntax-case f ()
                                 [(_ field-id _)  #'field-id])))
           ; Create a constructor for this interface.
           (define (intf-ctor-id)
             ; Call the default constructor and initialize each field.
             (intf-id #,@(for/list ([f (syntax->list #'(field ...))])
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
                                #'(field-intf-ctor-id))]))))))]))

; Get a proxy to a signal from an interface of the current component.
; Transforms: (interface-ref a b c d)
; Into:       (signal-proxy (unbox (d (c (b a)))))
(define-syntax interface-ref
  (syntax-rules ()
    [(interface-ref x)
     (signal-proxy (unbox x))]
    [(interface-ref a b c ...)
     (interface-ref (b a) c ...)]))

; Assign a signal to a field in the interface of the current component.
; Transforms: (interface-set! a b c d y)
; Into:       (set-box! (d (c (b a)))) y)
(define-syntax interface-set!
  (syntax-rules ()
    [(interface-set! x y)
     (set-box! x y)]
    [(interface-set! a b c ... y)
     (interface-set! (b a) c ... y)]))
