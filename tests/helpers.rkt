#lang racket

(require
  rackunit
  syntax/parse
  syntax/parse/define
  (only-in silicate signal-take))

(provide
  check-sig-equal?
  port-ref
  port-set!)

(define (check-sig-equal? t e n)
  (check-equal? (signal-take t n) (signal-take e n)))

(define-syntax-parser port-ref*
  [(port-ref* x)                    #'x]
  [(port-ref* x f:identifier i ...) #'(port-ref* (f x) i ...)]
  [(port-ref* x n:number i ...)     #'(port-ref* (vector-ref x n) i ...)])

(define-simple-macro (port-ref path ...)
  (unbox (port-ref* path ...)))

(define-simple-macro (port-set! (path ...) value)
  (set-box! (port-ref* path ...) value))
