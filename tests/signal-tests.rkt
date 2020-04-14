
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
      (letrec ([l (range 1 5)]
               [s (list->signal l)])
        (check-equal? l (signal-take s (length l)))
        (check-pred static? (signal-drop s (sub1 (length l))))))

    (test-case "Can delay a signal"
      (letrec ([l (range 1 5)]
               [s1 (list->signal l)]
               [s2 (register -1 s1)])
        (check-eq? -1 (signal-first s2))
        (check-eq? s1 (signal-rest s2))))

    (test-case "Can add1 to a signal"
      (letrec ([l (range 1 5)]
               [s (add1~ (list->signal l))])
        (check-equal? (map add1 l) (signal-take s (length l)))))

    (test-case "Can sub1 to a signal"
      (letrec ([l (range 1 5)]
               [s (sub1~ (list->signal l))])
        (check-equal? (map sub1 l) (signal-take s (length l)))))

    (test-case "Can add signals"
      (letrec ([l1 (range 1   5   1)]
               [l2 (range 10  50  10)]
               [l3 (range 100 500 100)]
               [s (+~ (list->signal l1) (list->signal l2) (list->signal l3))])
        (check-equal? (map + l1 l2 l3) (signal-take s (length l1)))))

    (test-case "Can negate a signal"
      (letrec ([l (range 1 5)]
               [s (-~ (list->signal l))])
        (check-equal? (map - l) (signal-take s (length l)))))

    (test-case "Can subtract signals"
      (letrec ([l1 (range 100 500 100)]
               [l2 (range 10  50  10)]
               [l3 (range 1   5   1)]
               [s (-~ (list->signal l1) (list->signal l2) (list->signal l3))])
        (check-equal? (map - l1 l2 l3) (signal-take s (length l1)))))

    (test-case "Can multiply signals"
      (letrec ([l1 (range 1   5   1)]
               [l2 (range 10  50  10)]
               [l3 (range 100 500 100)]
               [s (*~ (list->signal l1) (list->signal l2) (list->signal l3))])
        (check-equal? (map * l1 l2 l3) (signal-take s (length l1)))))

    (test-case "Can use if on signals"
      (letrec ([l1 (list #f #f #t #t)]
               [l2 (range 10 50 10)]
               [l3 (range 1  5  1)]
               [s (if~ (list->signal l1) (list->signal l2) (list->signal l3))])
        (check-equal? (map (lambda (c x y) (if c x y)) l1 l2 l3) (signal-take s (length l1)))))

    (test-case "Can register a signal"
      (letrec ([e  (list #f #t #f #t #t #f)]
               [l1 (list 10 20 30 40 50 60)]
               [l2 (list 0  0  20 20 40 50)]
               [s  (register/e 0 (list->signal e) (list->signal l1))])
        (check-equal? l2 (signal-take s (length l2)))))

    (test-case "Can create a simple counter"
      (letrec ([n 10]
               [s (register 0 (add1~ s))])
        (check-equal? (range n) (signal-take s n))))

    (test-case "Can create a modulo counter"
      (letrec ([n1 4] [n2 (* 3 n1)]
               [s (register 0 (if~ (=~ s (static (sub1 n1))) (static 0) (add1~ s)))])
        (check-equal? (build-list n2 (lambda (x) (remainder x n1)))
                      (signal-take s n2))))

    (test-case "Can cascade counters"
      (letrec ([n1 4] [n2 (* 5 n1)]
               [s1 (register 0 (if~ e (static 0) (add1~ s1)))]
               [e (=~ s1 (static (sub1 n1)))]
               [s2 (register/e 0 e (add1~ s2))])
        (check-equal? (build-list n2 (lambda (x) (quotient x n1)))
                      (signal-take s2 n2))))

    (test-case "Can create a counter as a Medvedev machine"
      (letrec ([tick (signal #f #t #f #t #t #f)]
               [s (medvedev 0 (lambda (n e) (if e (add1 n) n)) tick)]
               [l (list 0 0 1 1 2 3 3)])
        (check-equal? l (signal-take s (length l)))))

    (test-case "Can generate pulses with a Mealy machine"
      (letrec ([tick (signal #f #t #f #t #t #f)]
               [s (mealy 0 (lambda (n e)
                             (if e
                                 (list (add1 n) (= n 1))
                                 (list n #f)))
                           tick)]
               [l (list #f #f #f #t #f #f #f)])
        (check-equal? l (signal-take s (length l)))))

    (test-case "Can generate pulses with a Moore machine"
      (letrec ([tick (signal #f #t #f #t #t #f)]
               [s (moore 0 (lambda (n e) (if e (add1 n) n))
                           (lambda (n)   (= n 1))
                           tick)]
               [l (list #f #f #t #t #f #f #f)])
        (check-equal? l (signal-take s (length l)))))

    (test-case "Can oversample a signal"
      (letrec ([t1 7] [t2 3]
               [s1 (list->signal (range 7))]
               [s2 (resample t1 t2 s1)]
               [l (list 0 1 1 2 2 3 3 3 4 4 5 5 6 6 6)])
        (check-equal? l (signal-take s2 (length l)))))

    (test-case "Can undersample a signal"
      (letrec ([t1 3] [t2 7]
               [s1 (list->signal (range 13))]
               [s2 (resample t1 t2 s1)]
               [l (list 0 3 5 7 10 12)])
        (check-equal? l (signal-take s2 (length l)))))))
