#lang racket

; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a pair (x0 . f) where
; - x0 is the first, or current sample
; - f  is a function that computes a signal with the next samples.

; Alias the car function to read the first sample of a signal.
(define head car)

; Getting the tail of a signal consists in evaluating the right part of the pair.
(define (tail x)
  ((cdr x)))

; Create a constant signal.
(define (const~ x0)
  (letrec ([s (cons x0 (lambda () s))]) s))

; Define a signal with an initial sample x0 and an expression expr
; that compute a signal with the following samples.
; The expression will be wrapped into a memoized lambda.
(define-syntax-rule (signal x0 expr)
  (cons x0
    (let ([res (void)])
      (lambda ()
        (cond [(void? res) (set! res expr)])
        res))))

; Return a list with the first n samples of a signal s.
; TODO Should we use for/fold to avoid recursion?
(define (signal->list n s)
  (if (<= n 0)
    empty
    (cons (head s) (signal->list (sub1 n) (tail s)))))

; Convert a list to a signal.
(define (list->signal l)
  (if (= (length l) 1)
    (const~ (first l))
    (signal (first l) (list->signal (rest l)))))

; Helpers to create lambda functions that work on signals.
(define-syntax lambda~
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(lambda~ (x ...) body ...)
     (letrec ([f (lambda (x ...) body ...)]
              [g (lambda (x ...)
                   (signal
                     (f (head x) ...)
                     (g (tail x) ...)))])
       g)]
    ; Convert a function f with unspecified arity.
    [(lambda~ f)
     (letrec ([g (lambda x
                   (signal
                     (apply f (map head x))
                     (apply g (map tail x))))])
       g)]
    ; Create a function with variable arity.
    [(lambda~ x body ...)
     (lambda~ (lambda x a body ...))]))

; Define functions that work on signals.
(define-syntax define~
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(define~ (name x ...) body ...)
     (define name (lambda~ (x ...) body ...))]
    ; Convert a function f with unspecified arity.
    [(define~ name f)
     (define name (lambda~ f))]
    ; Create a function with variable arity.
    [(define~ (name . x) body ...)
     (define name (lambda~ x body ...))]))

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

(define~ =~ =)

; Create a signal that is the result of f
; and insert it as the first argument of f.
(define-syntax-rule (feedback-first y0 (f x ...))
  (letrec ([y (signal y0 (f y x ...))]) y))

; Create a signal that is the result of f
; and append it as the last argument of f.
(define-syntax-rule (feedback-last y0 (f x ...))
  (letrec ([y (signal y0 (f x ... y))]) y))

; Register a signal d with e as the enable input.
(define-syntax-rule (register q0 e d)
  (feedback-last q0 (if~ e d)))

; Transform a plain function into a Medvedev machine.
; medvedev : ∀(s i) s (s i -> s) (Signal i) -> (Signal s)
(define (medvedev s0 f x)
  (feedback-first s0 ((lambda~ f) x)))

; Transform a plain function into a Mealy machine.
; TODO use pairs instead of lists?
; mealy : ∀(s i o) s (s i -> (List s o)) (Signal i) -> (Signal o)
(define (mealy s0 f x)
  (letrec ([s  (signal s0 (first~ so))]
           [so ((lambda~ f) s  x)])
    (second~ so)))

; Transform a pair of functions into a Moore machine.
; moore : ∀(s i o) s (s i -> s) (s -> o) (Signal i) -> (Signal o)
(define (moore s0 f g x)
  ((lambda~ g) (medvedev s0 f x)))

; This function is translated from the veryUnsafeSynchronizer function in Cλash.
; It assumes the following timing for signals:
; * x[n] is the value of x for (n-1)×t1 < t ≤ n×t1
; * The result y[m] is the value of x for t = m×t2
(define (cross-domain t1 t2 x)
  (if (= t1 t2)
    ; If periods are the same, return x.
    ; In Cλash, a type conversion from source to target domain is performed.
    x
    (letrec ([cross-domain-at (lambda (t u)
                                (if (<= t 0)
                                  (signal (head u) (cross-domain-at (+ t t2) u))
                                  (cross-domain-at (- t t1) (tail u))))])
      (cross-domain-at 0 x))))

; Example: a constant signal with value 42
(signal->list 24 (const~ 42))

; Example: a signal defined from a list
(signal->list 8 (list->signal (list 10 20 30 40)))

; Example: a mod-5 counter.
(define counter1
  (signal 0 (if~ tick1
              (const~ 0)
              (add1~ counter1))))

(define tick1
  (=~ counter1 (const~ 4)))

(signal->list 24 counter1)

; Example: a mod-3 counter driven by counter1
(define counter2
  (register 0 tick1
    (if~ (=~ counter2 (const~ 2))
      (const~ 0)
      (add1~ counter2))))

(signal->list 24 counter2)

; Example: a counter defined as a medvedev machine
(define counter3
  (medvedev 0 (lambda (n e)
                (if e (add1 n) n)) tick1))

(signal->list 24 counter3)

; Example: pulse generator as a mealy machine
(define pulse
  (mealy 0 (lambda (n e)
             (if e (list (add1 n) (= n 2)) (list n #f))) tick1))

(signal->list 24 pulse)

; Example: use a function with a variable number of arguments
(define sum123 (+~ counter1 counter2 counter3))

(signal->list 24 sum123)

; Example: cross-domain signal resampling
(define counter1-over (cross-domain 7 3 counter1))
(define counter1-sub (cross-domain 3 7 counter1))

(signal->list 24 counter1-over)
(signal->list 24 counter1-sub)
