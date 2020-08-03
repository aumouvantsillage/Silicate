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
      (define c-i-a-0 (static 10))
      (define c-i-a-1 (static 20))
      (set-box! (Iab-a (vector-ref (CIab2-i c) 0)) c-i-a-0)
      (set-box! (Iab-a (vector-ref (CIab2-i c) 1)) c-i-a-1)

      (define c-i-b-0 (unbox (Iab-b (vector-ref (CIab2-i c) 0))))
      (define c-i-b-1 (unbox (Iab-b (vector-ref (CIab2-i c) 1))))
      (check-equal? (signal-take c-i-b-0 5) (signal-take c-i-a-0 5))
      (check-equal? (signal-take c-i-b-1 5) (signal-take c-i-a-1 5)))))
