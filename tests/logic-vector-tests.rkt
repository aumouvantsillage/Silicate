
#lang racket

(require rackunit)
(require "../src/logic-vector.rkt")

(provide logic-vector-tests)

(define logic-vector-tests
  (test-suite "Logic vectors"
    (test-case "Can read a bit in a vector filled with zeros"
      (define n 12)
      (define v (make-logic-vector n))
      (for ([i (range n)])
           (check-false (logic-vector-ref v i))))

    (test-case "Can read a bit in a vector filled with ones"
      (define n 12)
      (define v (make-logic-vector n #t))
      (for ([i (range n)])
           (check-true (logic-vector-ref v i))))))
