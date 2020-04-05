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
     (letrec ([r (list x0 (lambda () r))]) r)]
    ; Define a signal with an initial sample x0 and an expression xs
    ; that compute a signal with the following samples.
    [(signal x0 xs)
     (list x0
           (let ([m empty])
             (lambda ()
               (cond [(empty? m) (set! m (list xs))])
               (first m))))]))

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

; Transform f into a combinatorial component with the given name.
(define-syntax-rule (define-comb (name xs ...) f)
  (define (name xs ...)
    (signal
        (f (first xs) ...)
        (name (signal-rest xs) ...))))

; Versions of standard functions as combinatorial components.
(define-comb (if/comb cs xs ys) if)
(define-comb (add1/comb  xs)    add1)
(define-comb (sub1/comb  xs)    sub1)
(define-comb (=/comb     xs ys) =)

; Register signal xs with es as the enable input.
(define-syntax-rule (register x0 es xs)
  (letrec ([rs (signal x0 (if/comb es xs rs))]) rs))

; TODO Medvedev, Moore, Mealy

; Example: a constant signal with value 42
(define a (signal 42))
(sample-n 240 a)

; Example: a mod-5 counter.
(define counter1
    (signal 0 (if/comb tick1
                (signal 0)
                (add1/comb counter1))))

(define tick1
    (=/comb counter1 (signal 4)))

(sample-n 240 counter1)

; Example: a mod-3 counter driven by counter1
(define counter2
    (register 0 tick1
        (if/comb (=/comb counter2 (signal 2))
          (signal 0)
          (add1/comb counter2))))

(sample-n 240 counter2)
