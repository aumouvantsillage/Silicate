#lang racket

; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a two-element list (x0 f) where
; - x0 is the first, or current sample
; - f  is a function that computes a signal with the following samples.
;
; This macro defines a signal as an initial value x0 and and expression
; xs that computes another signal. xs is wrapped in a lambda to make f.
;
; For circuits with a feedback loop, computing the n-th sample will
; execute the n-th function, that will call the (n-1)-th function
; recursively, etc.
; There is a chance that the same sample is computed multiple times and
; stack space will be wasted. For this reason, the result of each function
; is memoized.
(define-syntax signal
  (syntax-rules ()
    ; Define a constant signal with value x0.
    ; For a constant signal s, (second s) will return s itself.
    [(signal x0)
     (letrec ([s (list x0 (lambda () s))]) s)]
    ; Define a signal with an initial sample x0 and an expression expr
    ; that compute a signal with the following samples.
    ; The expression will be wrapped into a memoized lambda.
    [(signal x0 expr)
     (list x0
           (let ([m (void)])
             (lambda ()
               (cond [(void? m) (set! m expr)])
               m)))]))

; Getting the rest of a signal consists in evaluating
; the second element of the signal object.
(define-syntax-rule (signal-rest xs)
  ((second xs)))

; Return a list with the first n samples of a signal.
; TODO Should we use for/fold to avoid recursion?
(define (sample-n n xs)
  (if (<= n 0)
    empty
    (cons (first xs) (sample-n (sub1 n) (signal-rest xs)))))

; Apply an n-ary function to n signals.
; (define (signal-map f . xss)
;   (signal
;       (apply f (map first xss))
;       (apply signal-map f (map signal-rest xss))))

; Lift f into a function from signals to signal.
; This is a macro because f is not always a function.
(define-syntax-rule (comb f xs ...)
  (letrec ([f/comb (lambda (xs ...)
                     (signal
                         (f (first xs) ...)
                         (f/comb (signal-rest xs) ...)))])
          f/comb))

; Versions of standard functions and special forms
; that work on signals.
(define if/comb     (comb if cs xs ys))
(define add1/comb   (comb add1 xs))
(define sub1/comb   (comb sub1 xs))
(define =/comb      (comb = xs ys))
(define first/comb  (comb first xs))
(define second/comb (comb second xs))

; Create a signal that is the result of f
; and insert it as the first argument of f before xs.
(define-syntax-rule (feedback/first x0 (f xs ...))
  (letrec ([ss (signal x0 (f ss xs ...))]) ss))

; Create a signal that is the result of f
; and append it as the last argument of f after xs.
(define-syntax-rule (feedback/last x0 (f xs ...))
  (letrec ([ss (signal x0 (f xs ... ss))]) ss))

; Register signal xs with es as the enable input.
(define-syntax-rule (register x0 es xs)
  (feedback/last x0 (if/comb es xs)))

; Transform a plain function into a Medvedev machine.
; medvedev :: s -> (s -> i -> s) -> (Signal i -> Signal s)
(define (medvedev s0 f)
  (let ([f/comb (comb f ss is)])
    (lambda (xs)
      (feedback/first s0 (f/comb xs)))))

; Transform a plain function into a Mealy machine.
; mealy :: s -> (s -> i -> (s, o)) -> (Signal i -> Signal o)
; TODO use values instead of lists?
(define (mealy s0 f)
  (let ([f/comb (comb f ss xs)])
    (lambda (xs)
      (letrec ([ss  (signal s0 (first/comb sos))]
               [sos (f/comb ss xs)])
              (second/comb sos)))))

(define (moore s0 f g)
  (let ([f/mdv (medvedev s0 f)]
        [g/comb (comb g ss)])
    (lambda (xs)
      (g/comb (f/mdv xs)))))

; Example: a constant signal with value 42
(define a (signal 42))
(sample-n 24 a)

; Example: a mod-5 counter.
(define counter1
    (signal 0 (if/comb tick1
                (signal 0)
                (add1/comb counter1))))

(define tick1
    (=/comb counter1 (signal 4)))

(sample-n 24 counter1)

; Example: a mod-3 counter driven by counter1
(define counter2
    (register 0 tick1
        (if/comb (=/comb counter2 (signal 2))
          (signal 0)
          (add1/comb counter2))))

(sample-n 24 counter2)

; Example: a counter defined as a medvedev machine
(define counter3
    ((medvedev 0 (lambda (n e)
                  (if e (add1 n) n))) tick1))

(sample-n 24 counter3)

; Example: pulse generator as a mealy machine
(define pulse
    ((mealy 0 (lambda (n e)
                (if e (list (add1 n) (= n 2)) (list n #f)))) tick1))

(sample-n 24 pulse)
