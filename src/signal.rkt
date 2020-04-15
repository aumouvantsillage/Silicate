#lang racket

(provide
    (contract-out
        [signal-first (-> signal? any)]
        [signal-rest  (-> signal? signal?)]
        [signal-drop  (-> signal? natural-number/c signal?)]
        [signal-take  (-> signal? natural-number/c (non-empty-listof any/c))]
        [static       (-> any/c   signal?)]
        [static?      (-> signal? boolean?)]
        [list->signal (-> (non-empty-listof any/c) signal?)]
        [rename signal* signal
                      (->* () () #:rest (non-empty-listof any/c) signal?)]
        [if~          (-> signal? signal? signal? signal?)]
        [add1~        (-> signal-of-number? signal-of-number?)]
        [sub1~        (-> signal-of-number? signal-of-number?)]
        [first~       (-> signal-of-list? signal?)]
        [second~      (-> signal-of-list? signal?)]
        [+~           (->* () () #:rest (listof           signal-of-number?) signal-of-number?)]
        [-~           (->* () () #:rest (non-empty-listof signal-of-number?) signal-of-number?)]
        [*~           (->* () () #:rest (listof           signal-of-number?) signal-of-number?)]
        [=~           (->* () () #:rest (non-empty-listof signal-of-number?) signal-of-boolean?)]
        [and~         (-> signal? signal? signal?)]
        [or~          (-> signal? signal? signal?)]
        [not~         (-> signal? signal?)]
        [medvedev     (-> any/c (-> any/c any/c any/c) signal?     signal?)]
        [mealy        (-> any/c (-> any/c any/c (non-empty-listof any/c)) signal? signal?)]
        [moore        (-> any/c (-> any/c any/c any/c) (-> any/c any/c)   signal? signal?)]
        [resample     (-> positive? positive? signal? signal?)]
        [signal-of    (-> (-> any/c boolean?) (-> signal? boolean?))])
    register
    register/r
    register/e
    register/re)


; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a pair (x0, f) where
; - x0 is the first, or current sample
; - f  is a function that computes a signal with the next samples.
(struct signal (first deferred-rest))

; Getting the rest of a signal consists in evaluating the right part of the pair.
(define (signal-rest x)
  ((signal-deferred-rest x)))

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
  (signal x0
    (let ([res #f])
      (λ ()
        (unless res (set! res expr))
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

(define (signal* . x)
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

; In Racket, and takes a variable number of arguments, but is not a function.
(define~ (and~ x y)
  (and x y))

; In Racket, or takes a variable number of arguments, but is not a function.
(define~ (or~ x y)
  (or x y))

(define~ (not~ x)
  (not x))

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

; For use in contracts. Returns a function that checks the given predicate on a signal.
(define (signal-of p?)
  (λ (s)
    (p? (signal-first s))))

; For use in contracts. Predicates to check various types of signals.
(define signal-of-number?  (signal-of number?))
(define signal-of-boolean? (signal-of boolean?))
(define signal-of-list?    (signal-of list?))
