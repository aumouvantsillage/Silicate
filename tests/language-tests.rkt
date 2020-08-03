#lang racket

(require
  rackunit
  silicate
  "language-fixtures.rkt")

(provide language-tests)

(define language-tests
  (test-suite "Language"
    (test-case "Can label simple signal expressions"
      (define c (make-instance-Cab))
      (define c-a (static 10))
      (set-box! (Cab-a c) c-a)

      (define c-b (unbox (Cab-b c)))
      (check-equal? (signal-take c-b 5) (signal-take c-a 5)))

    (test-case "Can resolve ports in field expressions"
      (define c (make-instance-CIab))
      (define c-i-a (static 10))
      (set-box! (Iab-a (CIab-i c)) c-i-a)

      (define c-i-b (unbox (Iab-b (CIab-i c))))
      (check-equal? (signal-take c-i-b 5) (signal-take c-i-a 5)))

    (test-case "Can resolve ports in indexed expressions"
      (define c (make-instance-CIab2))
      (define c-i-0-a (static 10))
      (define c-i-1-a (static 20))
      (set-box! (Iab-a (vector-ref (CIab2-i c) 0)) c-i-0-a)
      (set-box! (Iab-a (vector-ref (CIab2-i c) 1)) c-i-1-a)

      (define c-i-0-b (unbox (Iab-b (vector-ref (CIab2-i c) 0))))
      (define c-i-1-b (unbox (Iab-b (vector-ref (CIab2-i c) 1))))
      (check-equal? (signal-take c-i-0-b 5) (signal-take c-i-0-a 5))
      (check-equal? (signal-take c-i-1-b 5) (signal-take c-i-1-a 5)))

    (test-case "Can resolve ports in a hierarchy of expressions"
      (define c (make-instance-CIIab2))
      (define c-j-0-i-0-a (static 10))
      (define c-j-0-i-1-a (static 20))
      (define c-j-1-i-0-a (static 30))
      (define c-j-1-i-1-a (static 40))
      (set-box! (Iab-a (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 0)) 0)) c-j-0-i-0-a)
      (set-box! (Iab-a (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 0)) 1)) c-j-0-i-1-a)
      (set-box! (Iab-a (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 1)) 0)) c-j-1-i-0-a)
      (set-box! (Iab-a (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 1)) 1)) c-j-1-i-1-a)

      (define c-j-0-i-0-b (unbox (Iab-b (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 0)) 0))))
      (define c-j-0-i-1-b (unbox (Iab-b (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 0)) 1))))
      (define c-j-1-i-0-b (unbox (Iab-b (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 1)) 0))))
      (define c-j-1-i-1-b (unbox (Iab-b (vector-ref (IIab2-i (vector-ref (CIIab2-j c) 1)) 1))))

      (check-equal? (signal-take c-j-0-i-0-b 5) (signal-take c-j-0-i-0-a 5))
      (check-equal? (signal-take c-j-0-i-1-b 5) (signal-take c-j-0-i-1-a 5))
      (check-equal? (signal-take c-j-1-i-0-b 5) (signal-take c-j-1-i-0-a 5))
      (check-equal? (signal-take c-j-1-i-1-b 5) (signal-take c-j-1-i-1-a 5)))

    (test-case "Can assign a literal to a signal"
      (define c (make-instance-Cal))
      (define c-a (unbox (Cal-a c)))
      (check-equal? (signal-take c-a 5) (signal-take (static 10) 5)))

    (test-case "Can assign a constant to a signal"
      (define c (make-instance-Cac))
      (define c-a (unbox (Cac-a c)))
      (check-equal? (signal-take c-a 5) (signal-take (static 10) 5)))

    (test-case "Can assign a static expression to a signal"
      (define c (make-instance-Cas))
      (define c-a (unbox (Cas-a c)))
      (check-equal? (signal-take c-a 5) (signal-take (static 11) 5)))))
