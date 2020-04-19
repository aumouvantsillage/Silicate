#lang racket

(provide
  (contract-out
    [signal/c     contract?]
    [signal-first (-> signal/c any)]
    [signal-rest  (-> signal/c signal/c)]
    [signal-drop  (-> signal/c exact-nonnegative-integer? signal/c)]
    [signal-take  (-> signal/c exact-nonnegative-integer? (non-empty-listof any/c))]
    [static       (-> any/c signal/c)]
    [static?      (-> signal/c boolean?)]
    [list->signal (-> (non-empty-listof any/c) signal/c)]
    [rename signal*
     signal       (->* () () #:rest (non-empty-listof any/c) signal/c)]
    [lift         (-> procedure? (->* () () #:rest (listof signal/c) signal/c))]
    [medvedev     (-> any/c (-> any/c any/c any) signal/c signal/c)]
    [mealy        (-> any/c (-> any/c any/c (non-empty-listof any/c)) signal/c signal/c)]
    [moore        (-> any/c (-> any/c any/c any) (-> any/c any) signal/c signal/c)]
    [resample     (-> exact-nonnegative-integer? exact-nonnegative-integer? signal/c signal/c)]
    [.if          (-> signal/c signal/c signal/c signal/c)])
  signal-proxy
  λ/lift
  define/lift
  register
  register/r
  register/e
  register/re)

; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a function that returns a pair (x0 . f) where
; - x0 is the first sample
; - f  is a signal.
(define signal/c
  (-> pair?))

; Return the first sample of a signal.
(define (signal-first x)
  (car (x)))

; Return the signal starting at the second sample of x.
(define (signal-rest x)
  (cdr (x)))

; Create a signal with an initial sample x0 and an expression expr
; that compute a signal with the following samples.
; Memoize the pair (x0 . expr) to prevent multiple evaluations
; for signals that contain feedback loops.
(define-syntax-rule (make-signal x0 expr)
  (let ([res #f])
    (λ ()
      (unless res (set! res (cons x0 expr)))
      res)))

; Create a signal as a proxy of another signal.
; This will be useful when we want to create signals that depend on other
; signals defined later. Especially in circuits with circular dependencies.
(define-syntax-rule (signal-proxy expr)
  (λ () (expr)))

; Create a signal that is fed back as the first argument of f:
; y = y0 . (f y x ...))
(define-syntax-rule (feedback-first y0 (f x ...))
  (letrec ([y (make-signal y0 (f y x ...))]) y))

; Create a signal that is fed back as the last argument of f:
; y = y0 . (f x ... y))
(define-syntax-rule (feedback-last y0 (f x ...))
  (letrec ([y (make-signal y0 (f x ... y))]) y))

; Create a signal that is fed back as the argument of f:
; y = y0 . (f y).
(define-syntax-rule (feedback y0 f)
  (feedback-first y0 (f)))

; Create a constant signal with value x0.
(define (static x0)
  (feedback x0 identity))

; Test whether a signal is static.
(define (static? x)
  (equal? x (signal-rest x)))

; Return a signal that starts at the n-th sample of x.
(define (signal-drop x n)
  (if (positive? n)
    (signal-drop (signal-rest x) (sub1 n))
    x))

; Return a list with the first n samples of a signal x.
(define (signal-take x n)
  (if (positive? n)
    (cons (signal-first x) (signal-take (signal-rest x) (sub1 n)))
    empty))

; Convert a non-empty list to a signal.
(define (list->signal lst)
  (if (= (length lst) 1)
    (static (first lst))
    (make-signal (first lst) (list->signal (rest lst)))))

; Convert the arguments to a signal.
(define (signal* . x)
  (list->signal x))

; Convert a function f into a function that works on signals.
(define (lift f)
  (define (g . x)
    (make-signal
      (apply f (map signal-first x))
      (apply g (map signal-rest  x))))
  g)

; Helpers to create lambda functions that work on signals.
(define-syntax λ/lift
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(λ/lift (x ...) body ...)
     (letrec ([f (λ (x ...) body ...)]
              [g (λ (x ...)
                   (make-signal
                     (f (signal-first x) ...)
                     (g (signal-rest x) ...)))])
       g)]
    ; Create a function with variable arity.
    [(λ/lift x body ...)
     (lift (λ x body ...))]))

; Helpers to define functions that work on signals.
(define-syntax define/lift
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(_ (name x ...) body ...)
     (define name (λ/lift (x ...) body ...))]
    ; Create a function with variable arity.
    [(_ (name . x) body ...)
     (define name (λ/lift x body ...))]
    ; Convert a function f with unspecified arity.
    [(_ name f)
     (define name (lift f))]))

; A version of if that works on signals.
(define/lift (.if c x y)
  (if c x y))

; Create a signal that delays d through a simple register.
(define-syntax-rule (register q0 d)
  (make-signal q0 d))

; Register with synchronous reset.
(define-syntax-rule (register/r q0 r d)
  (register q0 (.if r (static q0) d)))

; Register with enable.
(define-syntax-rule (register/e q0 e d)
  (feedback-last q0 (.if e d)))

; Register with synchronous reset and enable.
(define-syntax-rule (register/re q0 r e d)
  (feedback q0 (λ (q) (.if r (static q0) (.if e d q)))))

; Transform a plain function into a Medvedev machine.
; medvedev : ∀(s i) s (s i -> s) (Signal i) -> (Signal s)
(define (medvedev s0 f x)
  (feedback-first s0 ((lift f) x)))

; Transform a plain function into a Mealy machine.
; mealy : ∀(s i o) s (s i -> (List s o)) (Signal i) -> (Signal o)
(define (mealy s0 f x)
  (letrec ([s  (register s0 ((lift first) so))]
           [so ((lift f) s  x)])
    ((lift second) so)))

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
