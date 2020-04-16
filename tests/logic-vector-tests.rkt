
#lang racket

(require rackunit)
(require "../src/logic-vector.rkt")

(provide logic-vector-tests)

(define logic-vector-tests
  (test-suite "Logic vectors"
    (test-case "Can read a bit in a vector filled with zeros"
      (define n 12)
      (define v (make-logic-vector n))
      (for ([i (in-range n)])
           (check-false (logic-vector-ref v i))))

    (test-case "Can read a bit in a vector filled with ones"
      (define n 12)
      (define v (make-logic-vector n #t))
      (for ([i (in-range n)])
           (check-true (logic-vector-ref v i))))

    (test-case "Out-of-bound access returns false"
      (define n 12)
      (define v (make-logic-vector n #t))
      (check-false (logic-vector-ref v n)))

    (test-case "Can create a vector from a list"
      (define l (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define v (list->logic-vector l))
      (for ([i (in-range (length l))])
           (check-eqv? (logic-vector-ref v i)
                       (list-ref l (- (length l) i 1)))))

    (test-case "Can convert a vector into a list"
      (define l (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define v (list->logic-vector l))
      (check-equal? (logic-vector->list v)
                    l))

    (test-case "Can create a vector from a string"
      (define s "101100101011110000")
      (define v (string->logic-vector s))
      (for ([i (in-range (string-length s))])
           (check-eqv? (logic-vector-ref v i)
                       (eqv? (string-ref s (- (string-length s) i 1)) #\1))))

    (test-case "Can convert a vector into a string"
      (define l (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define v (list->logic-vector l))
      (check-equal? (logic-vector->string v)
                    "101100101011110000"))))
