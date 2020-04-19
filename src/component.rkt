#lang racket

(require (for-syntax racket/syntax))
(require "signal.rkt")

(provide
    interface
    interface-ref
    interface-set!)

; Create a plain interface object as a struct of boxes.
; TODO hierarchical ports.
(define-syntax (interface stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (with-syntax ([make-name (format-id #'name "make-~a" #'name)])
       #'(begin
           (struct name (field ...))
           (define (make-name)
             (name (box (void 'field)) ...))))]))

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
