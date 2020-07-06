#lang racket

(require "signal.rkt")

; Versions of standard functions and special forms that work on signals.

(provide
  (contract-out
    [.add1       (-> signal/c signal/c)]
    [.sub1       (-> signal/c signal/c)]
    [.remainder  (-> signal/c signal/c signal/c)]
    [.zero?      (-> signal/c signal/c)]
    [.first      (-> signal/c signal/c)]
    [.second     (-> signal/c signal/c)]
    [.+          (->* () () #:rest (listof signal/c) signal/c)]
    [.-          (->* () () #:rest (listof signal/c) signal/c)]
    [.*          (->* () () #:rest (listof signal/c) signal/c)]
    [.=          (->* () () #:rest (listof signal/c) signal/c)]
    [.<          (->* () () #:rest (listof signal/c) signal/c)]
    [.and        (-> signal/c signal/c signal/c)]
    [.or         (-> signal/c signal/c signal/c)]
    [.not        (-> signal/c signal/c)]
    [.xor        (-> signal/c signal/c signal/c)]
    [.vector-ref (-> signal/c signal/c signal/c)]
    [.vector-set (-> signal/c signal/c signal/c signal/c)]))

(define/lift (.add1 x)
  (add1 x))

(define/lift (.sub1 x)
  (sub1 x))

(define/lift (.zero? x)
  (zero? x))

(define/lift (.remainder x y)
  (remainder x y))

(define/lift (.first x)
  (first x))

(define/lift (.second x)
  (second x))

(define/lift .+ +)
(define/lift .- -)
(define/lift .* *)
(define/lift .= =)
(define/lift .< <)

; In Racket, and takes a variable number of arguments, but is not a function.
(define/lift (.and x y)
  (and x y))

; In Racket, or takes a variable number of arguments, but is not a function.
(define/lift (.or x y)
  (or x y))

(define/lift (.not x)
  (not x))

(define/lift (.xor x y)
  (xor x y))

(define/lift (.vector-ref vec pos)
  (vector-ref vec pos))

(define/lift (.vector-set vec pos val)
  (define res (vector-copy vec))
  (vector-set! res pos val)
  res)
