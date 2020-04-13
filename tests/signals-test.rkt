
#lang racket

(require "../src/signals.rkt")

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
