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
      (check-equal? (signal-take c-b 5) (signal-take c-a 5)))))
