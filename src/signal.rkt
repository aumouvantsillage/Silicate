#lang racket

; TODO add contracts
(provide (all-defined-out))

; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a pair (x0 . f) where
; - x0 is the first, or current sample
; - f  is a function that computes a signal with the next samples.

; Alias the car function to read the first sample of a signal.
(define signal-first car)

; Getting the rest of a signal consists in evaluating the right part of the pair.
(define (signal-rest x)
  ((cdr x)))

(define (signal-drop x n)
  (if (positive? n)
    (signal-drop (signal-rest x) (sub1 n))
    x))

; Return a list with the first n samples of a signal s.
; TODO Should we use for/fold to avoid recursion?
(define (signal-take s n)
  (if (positive? n)
    (cons (signal-first s) (signal-take (signal-rest s) (sub1 n)))
    empty))

; Define a signal with an initial sample x0 and an expression expr
; that compute a signal with the following samples.
; The expression will be wrapped into a memoized lambda.
(define-syntax-rule (make-signal x0 expr)
  (cons x0
    (let ([res (void)])
      (λ ()
        (when (void? res) (set! res expr))
        res))))

; Create a signal that is the result of f
; and insert it as the first argument of f.
(define-syntax-rule (feedback-first y0 (f x ...))
  (letrec ([y (make-signal y0 (f y x ...))]) y))

; Create a signal that is the result of f
; and append it as the last argument of f.
(define-syntax-rule (feedback-last y0 (f x ...))
  (letrec ([y (make-signal y0 (f x ... y))]) y))

(define (feedback y0 f)
  (feedback-first y0 (f)))

; Create a static signal.
(define (static x0)
  (feedback x0 identity))

; Test whether a signal is static.
(define (static? s)
  (eq? s (signal-rest s)))

; Convert a list to a signal.
; FIXME Error on empty list
(define (list->signal l)
  (if (= (length l) 1)
    (static (first l))
    (make-signal (first l) (list->signal (rest l)))))

(define (signal . x)
  (list->signal x))

; Helpers to create lambda functions that work on signals.
(define-syntax λ~
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(λ~ (x ...) body ...)
     (letrec ([f (λ (x ...) body ...)]
              [g (λ (x ...)
                   (make-signal
                     (f (signal-first x) ...)
                     (g (signal-rest x) ...)))])
       g)]
    ; Convert a function f with unspecified arity.
    [(λ~ f)
     (letrec ([g (λ x
                   (make-signal
                     (apply f (map signal-first x))
                     (apply g (map signal-rest x))))])
       g)]
    ; Create a function with variable arity.
    [(λ~ x body ...)
     (λ~ (λ x a body ...))]))

; Define functions that work on signals.
(define-syntax define~
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(define~ (name x ...) body ...)
     (define name (λ~ (x ...) body ...))]
    ; Convert a function f with unspecified arity.
    [(define~ name f)
     (define name (λ~ f))]
    ; Create a function with variable arity.
    [(define~ (name . x) body ...)
     (define name (λ~ x body ...))]))

; Versions of standard functions and special forms that work on signals.
(define~ (if~ c x y)
  (if c x y))

(define~ (add1~ x)
  (add1 x))

(define~ (sub1~ x)
  (sub1 x))

(define~ (first~ x)
  (first x))

(define~ (second~ x)
  (second x))

(define~ +~ +)
(define~ -~ -)
(define~ *~ *)
(define~ =~ =)

; Simple register.
(define-syntax-rule (register q0 d)
  (make-signal q0 d))

; Register with synchronous reset.
(define-syntax-rule (register/r q0 r d)
  (register q0 (if~ r q0 d)))

; Register with enable.
(define-syntax-rule (register/e q0 e d)
  (feedback-last q0 (if~ e d)))

; Register with synchronous reset and enable.
(define-syntax-rule (register/re q0 r e d)
  (feedback q0 (λ (q) (if~ r q0 (if~ e d q)))))

; Transform a plain function into a Medvedev machine.
; medvedev : ∀(s i) s (s i -> s) (Signal i) -> (Signal s)
(define (medvedev s0 f x)
  (feedback-first s0 ((λ~ f) x)))

; Transform a plain function into a Mealy machine.
; TODO use pairs instead of lists?
; mealy : ∀(s i o) s (s i -> (List s o)) (Signal i) -> (Signal o)
(define (mealy s0 f x)
  (letrec ([s  (register s0 (first~ so))]
           [so ((λ~ f) s  x)])
    (second~ so)))

; Transform a pair of functions into a Moore machine.
; moore : ∀(s i o) s (s i -> s) (s -> o) (Signal i) -> (Signal o)
(define (moore s0 f g x)
  ((λ~ g) (medvedev s0 f x)))

; This function is translated from the veryUnsafeSynchronizer function in Cλash.
; It assumes the following timing for signals:
; * x[n] is the value of x for (n-1)×t1 < t ≤ n×t1
; * The result y[m] is the value of x for t = m×t2
(define (resample t1 t2 x)
  (if (= t1 t2)
    ; If periods are the same, return x.
    ; In Cλash, a type conversion from source to target domain is performed.
    x
    (letrec ([resample-relative (λ (t u)
                                  (if (positive? t)
                                    (resample-relative (- t t1) (signal-rest u))
                                    (make-signal (signal-first u) (resample-relative (+ t t2) u))))])
      (resample-relative 0 x))))
