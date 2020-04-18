#lang racket

(provide
    signal-first
    signal-rest
    signal-drop
    signal-take
    static
    static?
    list->signal
    (rename-out [signal* signal])
    lift
    if~
    add1~
    sub1~
    remainder~
    zero?~
    first~
    second~
    +~
    -~
    *~
    =~
    <~
    and~
    or~
    not~
    xor~
    vector-ref~
    vector-set~
    medvedev
    mealy
    moore
    resample
    signal-of
    signal-alias
    λ~
    define~
    register
    register/r
    register/e
    register/re)


; A signal represents an infinite list of values.
;
; In Silicate, a signal item is defined as a pair (x0, f) where
; - x0 is the current sample
; - f  is a function that computes the next item.
;
; A signal itself is a function that returns a signal item.
(struct signal-item (first rest))

; Return the first element of a signal item.
(define (signal-first x)
  (signal-item-first (x)))

(define (signal-rest x)
  (signal-item-rest (x)))

(define (signal-drop x n)
  (if (positive? n)
    (signal-drop (signal-rest x) (sub1 n))
    x))

; Return a list with the first n samples of a signal s.
(define (signal-take s n)
  (if (positive? n)
    (cons (signal-first s) (signal-take (signal-rest s) (sub1 n)))
    empty))

; Define a signal with an initial sample x0 and an expression expr
; that compute a signal with the following samples.
; The expression will be wrapped into a memoized lambda.
(define-syntax-rule (make-signal x0 expr)
  (let ([res #f])
    (λ ()
      (unless res (set! res (signal-item x0 expr)))
      res)))

(define-syntax-rule (signal-alias expr)
  (λ () (expr)))

; Create a signal y = (f y x ...)
(define-syntax-rule (feedback-first y0 (f x ...))
  (letrec ([y (make-signal y0 (f y x ...))]) y))

; Create a signal y = (f x ... y)
(define-syntax-rule (feedback-last y0 (f x ...))
  (letrec ([y (make-signal y0 (f x ... y))]) y))

; Create a signal y = (f y).
(define-syntax-rule (feedback y0 f)
  (feedback-first y0 (f)))

; Create a static signal.
(define (static x0)
  (feedback x0 identity))

; Test whether a signal is static.
(define (static? s)
  (eq? s (signal-rest s)))

; Convert a non-empty list to a signal.
(define (list->signal l)
  (if (= (length l) 1)
    (static (first l))
    (make-signal (first l) (list->signal (rest l)))))

; Convert the arguments to a signal.
(define (signal* . x)
  (list->signal x))

; Convert a function f with unspecified arity.
(define (lift f)
  (letrec ([g (λ x
                (make-signal
                  (apply f (map signal-first x))
                  (apply g (map signal-rest x))))])
    g))

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
    ; Create a function with variable arity.
    [(λ~ x body ...)
     (lift (λ x body ...))]))

; Define functions that work on signals.
(define-syntax define~
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(define~ (name x ...) body ...)
     (define name (λ~ (x ...) body ...))]
    ; Create a function with variable arity.
    [(define~ (name . x) body ...)
     (define name (λ~ x body ...))]
    ; Convert a function f with unspecified arity.
    [(define~ name f)
     (define name (lift f))]))

; Versions of standard functions and special forms that work on signals.
(define~ (if~ c x y)
  (if c x y))

(define~ (add1~ x)
  (add1 x))

(define~ (sub1~ x)
  (sub1 x))

(define~ (zero?~ x)
  (zero? x))

(define~ (remainder~ x y)
  (remainder x y))

(define~ (first~ x)
  (first x))

(define~ (second~ x)
  (second x))

(define~ +~ +)
(define~ -~ -)
(define~ *~ *)
(define~ =~ =)
(define~ <~ <)

; In Racket, and takes a variable number of arguments, but is not a function.
(define~ (and~ x y)
  (and x y))

; In Racket, or takes a variable number of arguments, but is not a function.
(define~ (or~ x y)
  (or x y))

(define~ (not~ x)
  (not x))

(define~ (xor~ x y)
  (xor x y))

(define~ (vector-ref~ vec pos)
  (vector-ref vec pos))

(define~ (vector-set~ vec pos val)
  (define res (vector-copy vec))
  (vector-set! res pos val)
  res)

; Simple register.
(define-syntax-rule (register q0 d)
  (make-signal q0 d))

; Register with synchronous reset.
(define-syntax-rule (register/r q0 r d)
  (register q0 (if~ r (static q0) d)))

; Register with enable.
(define-syntax-rule (register/e q0 e d)
  (feedback-last q0 (if~ e d)))

; Register with synchronous reset and enable.
(define-syntax-rule (register/re q0 r e d)
  (feedback q0 (λ (q) (if~ r (static q0) (if~ e d q)))))

; Transform a plain function into a Medvedev machine.
; medvedev : ∀(s i) s (s i -> s) (Signal i) -> (Signal s)
(define (medvedev s0 f x)
  (feedback-first s0 ((lift f) x)))

; Transform a plain function into a Mealy machine.
; TODO use pairs instead of lists?
; mealy : ∀(s i o) s (s i -> (List s o)) (Signal i) -> (Signal o)
(define (mealy s0 f x)
  (letrec ([s  (register s0 (first~ so))]
           [so ((lift f) s  x)])
    (second~ so)))

; Transform a pair of functions into a Moore machine.
; moore : ∀(s i o) s (s i -> s) (s -> o) (Signal i) -> (Signal o)
(define (moore s0 f g x)
  ((lift g) (medvedev s0 f x)))

; This function is translated from the veryUnsafeSynchronizer function in Cλash.
; It assumes the following timing for signals:
; * x[n] is the value of x for (n-1)×t1 < t ≤ n×t1
; * The result y[m] is the value of x for t = m×t2
(define (resample t1 t2 x)
  (if (= t1 t2)
    ; If periods are the same, return x.
    ; In Cλash, a type conversion from source to target domain is performed.
    x
    (let resample-relative ([t 0] [u x])
      (if (positive? t)
        (resample-relative (- t t1) (signal-rest u))
        (make-signal (signal-first u) (resample-relative (+ t t2) u))))))

; For use in contracts. Returns a function that checks the given predicate on a signal.
(define (signal-of p?)
  (λ (s)
    (p? (signal-first s))))

; For use in contracts. Predicates to check various types of signals.
(define signal-of-number?  (signal-of number?))
(define signal-of-integer? (signal-of integer?))
(define signal-of-natural? (signal-of exact-nonnegative-integer?))
(define signal-of-boolean? (signal-of boolean?))
(define signal-of-list?    (signal-of list?))
(define signal-of-vector?  (signal-of vector?))
