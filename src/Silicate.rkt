#lang racket

; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a pair (x0 . f) where
; - x0 is the first, or current sample
; - f  is a function that computes a signal with the following samples.
;
; The second rule defines a signal as an initial value x0 and and expression
; expr that computes another signal. expr is wrapped in a lambda to make f.
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
     (letrec ([s (cons x0 (lambda () s))]) s)]
    ; Define a signal with an initial sample x0 and an expression expr
    ; that compute a signal with the following samples.
    ; The expression will be wrapped into a memoized lambda.
    [(signal x0 expr)
     (cons x0
           (let ([m (void)])
             (lambda ()
               (cond [(void? m) (set! m expr)])
               m)))]))

; Alias the car function to read the first sample of a signal.
(define head car)

; Getting the tail of a signal consists in evaluating
; the right part of the pair.
(define (tail s)
  ((cdr s)))

; Return a list with the first n samples of a signal.
; TODO Should we use for/fold to avoid recursion?
(define (sample-n n s)
  (if (<= n 0)
    empty
    (cons (car s) (sample-n (sub1 n) (tail s)))))

; Apply an n-ary function to n signals.
(define (map~ f . s)
  (signal
      (apply f      (map head s))
      (apply map~ f (map tail s))))

; Lift f into a function from signals to signal.
; This is a macro because f is not always a function.
(define-syntax lift
  (syntax-rules ()
    ; Lift a function with a possibly variable number of arguments.
    [(lift f)
     (lambda x (apply map~ f x))]
    ; Lift a function, macro or special form with a known arity.
    ; This should be more efficient than map~.
    [(lift f s ...)
     (letrec ([f~ (lambda (s ...)
                     (signal
                         (f  (head s) ...)
                         (f~ (tail s) ...)))])
          f~)]))

; Versions of standard functions and special forms
; that work on signals.
(define if~     (lift if     c x y))
(define add1~   (lift add1   x))
(define sub1~   (lift sub1   x))
(define first~  (lift first  x))
(define second~ (lift second x))
(define +~      (lift +))
(define =~      (lift =))

; Create a signal that is the result of f
; and insert it as the first argument of f before s.
(define-syntax-rule (feedback-first y0 (f x ...))
  (letrec ([y (signal y0 (f y x ...))]) y))

; Create a signal that is the result of f
; and append it as the last argument of f after s.
(define-syntax-rule (feedback-last y0 (f x ...))
  (letrec ([y (signal y0 (f x ... y))]) y))

; Register signal s with e as the enable input.
(define-syntax-rule (register q0 e d)
  (feedback-last q0 (if~ e d)))

; Transform a plain function into a Medvedev machine.
; medvedev :: s -> (s -> i -> s) -> (Signal i -> Signal s)
(define (medvedev s0 f)
  (let ([f~ (lift f s x)])
    (lambda (x)
      (feedback-first s0 (f~ x)))))

; Transform a plain function into a Mealy machine.
; mealy :: s -> (s -> i -> (s, o)) -> (Signal i -> Signal o)
; TODO use values instead of lists?
(define (mealy s0 f)
  (let ([f~ (lift f s x)])
    (lambda (x)
      (letrec ([s  (signal s0 (first~ so))]
               [so (f~ s x)])
              (second~ so)))))

(define (moore s0 f g)
  (let ([f~ (medvedev s0 f)]
        [g~ (lift g s)])
    (lambda (x)
      (g~ (f~ x)))))

; Example: a constant signal with value 42
(define a (signal 42))
(sample-n 24 a)

; Example: a mod-5 counter.
(define counter1
    (signal 0 (if~ tick1
                (signal 0)
                (add1~ counter1))))

(define tick1
    (=~ counter1 (signal 4)))

(sample-n 24 counter1)

; Example: a mod-3 counter driven by counter1
(define counter2
    (register 0 tick1
        (if~ (=~ counter2 (signal 2))
          (signal 0)
          (add1~ counter2))))

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

; Example: use a function with a variable number of arguments
(define sum123 (+~ counter1 counter2 counter3))

(sample-n 24 sum123)
