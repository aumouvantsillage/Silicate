#lang typed/racket ;/no-check

; A signal represents an infinite list of values.
;
; In Silicate, a signal is defined as a pair (x0 . f) where
; - x0 is the first, or current sample
; - f  is a function that computes a signal with the next samples.
(define-type (Signal a) (Pairof a (-> (Signal a))))

; Alias the cons function to create a signal.
; Typed Racket will not accept (define make-signal cons)
(: make-signal : All (a) a (-> (Signal a)) -> (Signal a))
(define (make-signal x0 f)
  (cons x0 f))

; Alias the car function to read the first sample of a signal.
; Typed Racket will not accept (define make-signal car)
(: head : All (a) (Signal a) -> a)
(define (head s)
  (car s))

; Getting the tail of a signal consists in evaluating the right part of the pair.
(: tail : All (a) (Signal a) -> (Signal a))
(define (tail s)
  ((cdr s)))

; Create a constant signal.
(: const~ : All (a) a -> (Signal a))
(define (const~ x0)
  (letrec ([s : (Signal a) (make-signal x0 (lambda () s))]) s))

; Define a signal with an initial sample x0 and an expression expr
; that compute a signal with the following samples.
; The expression will be wrapped into a memoized lambda.
;
; The flag and initial value of res have been introduced to satify the
; type checker. Attempts to use the Option type from Typed Racket with
; occurrence typing failed.
(define-syntax-rule (signal x0 expr)
    (make-signal x0
        (let ([res : (Option (Signal Any)) #f])
          (lambda ()
            (cond [(not res) (set! res expr)])
            res))))

; Return a list with the first n samples of a signal s.
; TODO Should we use for/fold to avoid recursion?
(: signal->list : All (a) Natural (Signal a) -> (Listof a))
(define (signal->list n s)
  (if (<= n 0)
    empty
    (cons (head s) (signal->list (sub1 n) (tail s)))))

(: list->signal : All (a) (Listof a) -> (Signal a))
(define (list->signal l)
  (if (= (length l) 1)
    (const~ (first l))
    (signal (first l) (list->signal (rest l)))))

(define-syntax lambda~
    (syntax-rules ()
      [(lambda~ (x ...) body ...)
       (letrec ([f : (All (a b (... ...)) (Signal b) (... ...) b -> (Signal a))
                     (lambda (x ...)
                       (signal
                         ((lambda (x ...) body ...) (head x) ...)
                         (f                         (tail x) ...)))])
               f)]
      [(lambda~ f)
       (letrec ([g (lambda x
                     (signal
                       (apply f (map head x))
                       (apply g (map tail x))))])
               g)]
      [(lambda~ x body ...)
       (lambda~ (lambda x a body ...))]))

(define-syntax define~
    (syntax-rules ()
      [(define~ (name x ...) body ...)
       (define name (lambda~ (x ...) body ...))]
      [(define~ name f)
       (define name (lambda~ f))]
      [(define~ (name . x) body ...)
       (define name (lambda~ x body ...))]))
#|
; Versions of standard functions and special forms
; that work on signals.
(: if~ : All (a) (Signal Boolean) (Signal a) (Signal a) -> (Signal a))
(define~ (if~ c x y)
  (if c x y))

(: add1~ : (Signal Number) -> (Signal Number))
(define~ (add1~ x)
  (add1 x))

(: sub1~ : (Signal Number) -> (Signal Number))
(define~ (sub1~ x)
  (sub1 x))

(: first~ : All (a b ...) (Signal (List a b ... b)) -> (Signal a))
(define~ (first~ x)
  (first x))

(: second~ : All (a b c ...) (Signal (List a b c ... c)) -> (Signal b))
(define~ (second~ x)
  (second x))

(: +~ : (Signal Number) * -> (Signal Number))
(define~ +~ +)

(: =~ : (Signal Number) * -> (Signal Boolean))
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
(: medvedev : All (s i) s (s i -> s) (Signal i) -> (Signal s))
(define (medvedev s0 f x)
  (feedback-first s0 ((lambda~ f) x)))

; Transform a plain function into a Mealy machine.
; TODO use pairs instead of lists?
(: mealy : All (s i o) s (s i -> (List s o)) (Signal i) -> (Signal o))
(define (mealy s0 f x)
  (letrec ([st (signal s0 (first~ so))]
           [so ((lambda~ f) st x)])
          (second~ so)))

; Transform a pair of functions into a Moore machine.
(: moore : All (s i o) s (s i -> s) (s -> o) (Signal i) -> (Signal o))
(define (moore s0 f g x)
  ((lambda~ g) (medvedev s0 f x)))

; This function is translated from the veryUnsafeSynchronizer function in Cλash.
; It assumes the following timing for signals:
; * x[n] is the value of x for (n-1)×t1 < t ≤ n×t1
; * The result y[m] is the value of x for t = m×t2
; We must make sure that register on y produces the expected signal.
(: cross-domain : All (a) Positive-Integer Positive-Integer (Signal a) -> (Signal a))
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
|#
; Example: a constant signal with value 42
(signal->list 24 (const~ 42))

; Example: a signal defined from a list
(signal->list 8 (list->signal (list 10 20 30 40)))
#|
; Example: a mod-5 counter.
(: counter1 : (Signal Integer))
(define counter1
    (signal 0 (if~ tick1
                (const~ 0)
                (add1~ counter1))))

(define tick1
    (=~ counter1 (const~ 4)))

(signal->list 24 counter1)

; Example: a mod-3 counter driven by counter1
(: counter2 : (Signal Integer))
(define counter2
    (register 0 tick1
        (if~ (=~ counter2 (const~ 2))
          (const~ 0)
          (add1~ counter2))))

(signal->list 24 counter2)

; Example: a counter defined as a medvedev machine
(: counter3 : (Signal Integer))
(define counter3
    (medvedev 0 (lambda (n e) (if e (add1 n) n)) tick1))

(signal->list 24 counter3)

; Example: pulse generator as a mealy machine
(: pulse : (Signal Boolean))
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
|#
