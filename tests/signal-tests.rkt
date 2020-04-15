
#lang racket

; TODO test register/r
; TODO test register/re

(require rackunit)
(require "../src/signal.rkt")

(provide signal-tests)

(define signal-tests
  (test-suite "Signals"
    (test-case "Static signal asserts static? predicate"
      (check-pred static? (static 42)))

    (test-case "Can read a constant signal"
      (check-equal?
        (signal-take (static 42) 4)
        (list 42 42 42 42)))

    (test-case "Can create a signal from a list"
      (define l (range 1 5))
      (define s (list->signal l))
      (check-equal? l (signal-take s (length l)))
      (check-pred static? (signal-drop s (sub1 (length l)))))

    (test-case "Can delay a signal"
      (define l (range 1 5))
      (define s1 (list->signal l))
      (define s2 (register -1 s1))
      (check-eq? -1 (signal-first s2))
      (check-eq? s1 (signal-rest s2)))

    (test-case "Can add1 to a signal"
      (define l (range 1 5))
      (define s (add1~ (list->signal l)))
      (check-equal? (map add1 l) (signal-take s (length l))))

    (test-case "Can sub1 to a signal"
      (define l (range 1 5))
      (define s (sub1~ (list->signal l)))
      (check-equal? (map sub1 l) (signal-take s (length l))))

    (test-case "Can add signals"
      (define l1 (range 1   5   1))
      (define l2 (range 10  50  10))
      (define l3 (range 100 500 100))
      (define s (+~ (list->signal l1) (list->signal l2) (list->signal l3)))
      (check-equal? (map + l1 l2 l3) (signal-take s (length l1))))

    (test-case "Can negate a signal"
      (define l (range 1 5))
      (define s (-~ (list->signal l)))
      (check-equal? (map - l) (signal-take s (length l))))

    (test-case "Can subtract signals"
      (define l1 (range 100 500 100))
      (define l2 (range 10  50  10))
      (define l3 (range 1   5   1))
      (define s (-~ (list->signal l1) (list->signal l2) (list->signal l3)))
      (check-equal? (map - l1 l2 l3) (signal-take s (length l1))))

    (test-case "Can multiply signals"
      (define l1 (range 1   5   1))
      (define l2 (range 10  50  10))
      (define l3 (range 100 500 100))
      (define s (*~ (list->signal l1) (list->signal l2) (list->signal l3)))
      (check-equal? (map * l1 l2 l3) (signal-take s (length l1))))

    (test-case "Can use if on signals"
      (define l1 (list #f #f #t #t))
      (define l2 (range 10 50 10))
      (define l3 (range 1  5  1))
      (define s (if~ (list->signal l1) (list->signal l2) (list->signal l3)))
      (check-equal? (map (λ (c x y) (if c x y)) l1 l2 l3) (signal-take s (length l1))))

    (test-case "Can register a signal"
      (define e  (list #f #t #f #t #t #f))
      (define l1 (list 10 20 30 40 50 60))
      (define l2 (list 0  0  20 20 40 50))
      (define s  (register/e 0 (list->signal e) (list->signal l1)))
      (check-equal? l2 (signal-take s (length l2))))

    (test-case "Can create a simple counter"
      (define n 10)
      (define s (register 0 (add1~ s)))
      (check-equal? (range n) (signal-take s n)))

    (test-case "Can create a modulo counter"
      (define n1 4)
      (define n2 (* 3 n1))
      (define s (register 0 (if~ (=~ s (static (sub1 n1))) (static 0) (add1~ s))))
      (check-equal? (build-list n2 (λ (x) (remainder x n1)))
                    (signal-take s n2)))

    (test-case "Can cascade counters"
      (define n1 4)
      (define n2 (* 5 n1))
      (define s1 (register 0 (if~ e (static 0) (add1~ s1))))
      (define e (=~ s1 (static (sub1 n1))))
      (define s2 (register/e 0 e (add1~ s2)))
      (check-equal? (build-list n2 (λ (x) (quotient x n1)))
                    (signal-take s2 n2)))

    (test-case "Can create a counter as a Medvedev machine"
      (define tick (signal #f #t #f #t #t #f))
      (define s (medvedev 0 (λ (n e) (if e (add1 n) n)) tick))
      (define l (list 0 0 1 1 2 3 3))
      (check-equal? l (signal-take s (length l))))

    (test-case "Can generate pulses with a Mealy machine"
      (define tick (signal #f #t #f #t #t #f))
      (define s (mealy 0 (λ (n e)
                            (if e
                                (list (add1 n) (= n 1))
                                (list n #f)))
                         tick))
      (define l (list #f #f #f #t #f #f #f))
      (check-equal? l (signal-take s (length l))))

    (test-case "Can generate pulses with a Moore machine"
      (define tick (signal #f #t #f #t #t #f))
      (define s (moore 0 (λ (n e) (if e (add1 n) n))
                         (λ (n)   (= n 1))
                         tick))
      (define l (list #f #f #t #t #f #f #f))
      (check-equal? l (signal-take s (length l))))

    (test-case "Can oversample a signal"
      (define t1 7)
      (define t2 3)
      (define s1 (list->signal (range 7)))
      (define s2 (resample t1 t2 s1))
      (define l (list 0 1 1 2 2 3 3 3 4 4 5 5 6 6 6))
      (check-equal? l (signal-take s2 (length l))))

    (test-case "Can undersample a signal"
      (define t1 3)
      (define t2 7)
      (define s1 (list->signal (range 13)))
      (define s2 (resample t1 t2 s1))
      (define l (list 0 3 5 7 10 12))
      (check-equal? l (signal-take s2 (length l))))))
