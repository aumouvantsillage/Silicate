#lang racket

(require
  rackunit
  silicate
  "language-fixtures.rkt")

(provide language-tests)

(define language-tests
  (test-suite "Language"
    (test-case "Can label simple signal expressions"
      (define c (make-instance-C0))
      (define c-x (static 10))
      (set-box! (C0-x c) c-x)

      (define c-y (unbox (C0-y c)))
      (check-equal? (signal-take c-y 5) (signal-take c-x 5)))

    (test-case "Can resolve ports in field expressions"
      (define c (make-instance-C1))
      (define c-i-x (static 10))
      (set-box! (I0-x (C1-i c)) c-i-x)

      (define c-i-y (unbox (I0-y (C1-i c))))
      (check-equal? (signal-take c-i-y 5) (signal-take c-i-x 5)))

    (test-case "Can resolve ports in indexed expressions"
      (define c (make-instance-C2))
      (define c-i-0-x (static 10))
      (define c-i-1-x (static 20))
      (set-box! (I0-x (vector-ref (C2-i c) 0)) c-i-0-x)
      (set-box! (I0-x (vector-ref (C2-i c) 1)) c-i-1-x)

      (define c-i-0-y (unbox (I0-y (vector-ref (C2-i c) 0))))
      (define c-i-1-y (unbox (I0-y (vector-ref (C2-i c) 1))))
      (check-equal? (signal-take c-i-0-y 5) (signal-take c-i-0-x 5))
      (check-equal? (signal-take c-i-1-y 5) (signal-take c-i-1-x 5)))

    (test-case "Can resolve ports in a hierarchy of expressions"
      (define c (make-instance-C3))
      (define c-j-0-i-0-x (static 10))
      (define c-j-0-i-1-x (static 20))
      (define c-j-1-i-0-x (static 30))
      (define c-j-1-i-1-x (static 40))
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 0)) 0)) c-j-0-i-0-x)
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 0)) 1)) c-j-0-i-1-x)
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 1)) 0)) c-j-1-i-0-x)
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 1)) 1)) c-j-1-i-1-x)

      (define c-j-0-i-0-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 0)) 0))))
      (define c-j-0-i-1-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 0)) 1))))
      (define c-j-1-i-0-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 1)) 0))))
      (define c-j-1-i-1-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 1)) 1))))

      (check-equal? (signal-take c-j-0-i-0-y 5) (signal-take c-j-0-i-0-x 5))
      (check-equal? (signal-take c-j-0-i-1-y 5) (signal-take c-j-0-i-1-x 5))
      (check-equal? (signal-take c-j-1-i-0-y 5) (signal-take c-j-1-i-0-x 5))
      (check-equal? (signal-take c-j-1-i-1-y 5) (signal-take c-j-1-i-1-x 5)))

    (test-case "Can assign a literal to a signal"
      (define c (make-instance-C4))
      (define c-x (unbox (C4-x c)))
      (check-equal? (signal-take c-x 5) (signal-take (static 10) 5)))

    (test-case "Can assign a constant to a signal"
      (define c (make-instance-C5))
      (define c-x (unbox (C5-x c)))
      (check-equal? (signal-take c-x 5) (signal-take (static 10) 5)))

    (test-case "Can assign a static expression to a signal"
      (define c (make-instance-C6))
      (define c-x (unbox (C6-x c)))
      (check-equal? (signal-take c-x 5) (signal-take (static 11) 5)))

    (test-case "Can lift an operation"
      (define c (make-instance-C7))
      (define c-x (list->signal (list 10 20 30)))
      (define c-y (list->signal (list 40 50 60)))
      (set-box! (C7-x c) c-x)
      (set-box! (C7-y c) c-y)
      (define c-z (unbox (C7-z c)))
      (check-equal? (signal-take c-z 5) (map + (signal-take c-x 5) (signal-take c-y 5))))

    (test-case "Can lift nested calls"
      (define c (make-instance-C8))
      (define c-x (list->signal (list 10 20 30 40 50)))
      (define c-y (static 2))
      (define c-z (list->signal (list 1 2 3 4 5)))
      (define c-u (static 3))
      (set-box! (C8-x c) c-x)
      (set-box! (C8-y c) c-y)
      (set-box! (C8-z c) c-z)
      (set-box! (C8-u c) c-u)
      (define c-v (unbox (C8-v c)))
      (check-equal? (signal-take c-v 5) (list 23 46 69 92 115)))

    (test-case "Can use local signals"
      (define c (make-instance-C9))
      (define c-x (list->signal (list 10 20 30 40 50)))
      (define c-y (static 2))
      (define c-z (list->signal (list 1 2 3 4 5)))
      (define c-u (static 3))
      (set-box! (C9-x c) c-x)
      (set-box! (C9-y c) c-y)
      (set-box! (C9-z c) c-z)
      (set-box! (C9-u c) c-u)
      (define c-v (unbox (C9-v c)))
      (check-equal? (signal-take c-v 5) (list 23 46 69 92 115)))

    (test-case "Can access simple ports in a vector composite port with dynamic indices"
      (define c (make-instance-C10))
      (define c-i-0-x (static 10))
      (define c-i-1-x (static 20))
      (define c-i-2-x (static 30))
      (set-box! (I2-x (vector-ref (C10-i c) 0)) c-i-0-x)
      (set-box! (I2-x (vector-ref (C10-i c) 1)) c-i-1-x)
      (set-box! (I2-x (vector-ref (C10-i c) 2)) c-i-2-x)

      (define c-y (list->signal (list 0 1 2 1 0 2)))
      (set-box! (C10-y c) c-y)

      (define c-z-lst (list 10 20 30 20 10 30))
      (define c-z (unbox (C10-z c)))
      (check-equal? (signal-take c-z (length c-z-lst)) c-z-lst))))
