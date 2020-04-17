
#lang racket

(require rackunit)
(require "../src/logic-vector.rkt")

(provide logic-vector-tests)

(define logic-vector-tests
  (test-suite "Logic vectors"
    (test-case "Can read a bit in a vector filled with zeros"
      (define n 12)
      (define lv (make-logic-vector n))
      (for ([i (in-range n)])
           (check-false (logic-vector-ref lv i))))

    (test-case "Can create a vector from a list"
      (define lst (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define lv (list->logic-vector lst))
      (for ([i (in-range (length lst))])
           (check-eqv? (logic-vector-ref lv i)
                       (list-ref lst (- (length lst) i 1)))))

    (test-case "Out-of-bound access returns false"
      (define lst (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define lv (list->logic-vector lst))
      (check-false (logic-vector-ref lv (length lst))))

    (test-case "Can convert a vector into a list"
      (define lst (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define lv (list->logic-vector lst))
      (check-equal? (logic-vector->list lv)
                    lst))

    (test-case "Can create a vector from a string"
      (define s "101100101011110000")
      (define lv (string->logic-vector s))
      (for ([i (in-range (string-length s))])
           (check-eqv? (logic-vector-ref lv i)
                       (eqv? (string-ref s (- (string-length s) i 1)) #\1))))

    (test-case "Can convert a vector into a string"
      (define lst (list #t #f #t #t #f #f #t #f #t #f #t #t #t #t #f #f #f #f))
      (define lv (list->logic-vector lst))
      (check-equal? (logic-vector->string lv)
                    "101100101011110000"))

    (test-case "Can create a vector from a positive integer"
      (define val 735)
      (define lv (integer->logic-vector val 12))
      (for ([i (in-range (logic-vector-length lv))])
           (check-eqv? (logic-vector-ref lv i)
                       (bitwise-bit-set? val i))))

    (test-case "Can create a vector from a negative integer"
      (define val -735)
      (define lv (integer->logic-vector val 12))
      (for ([i (in-range (logic-vector-length lv))])
           (check-eqv? (logic-vector-ref lv i)
                       (bitwise-bit-set? val i))))

    (test-case "Can convert an 8-bit vector into a positive integer"
      (define val 73)
      (define lv (integer->logic-vector val 8))
      (check-eqv? (logic-vector->integer lv)
                  val))

    (test-case "Can convert a 12-bit vector into a positive integer"
      (define val 735)
      (define lv (integer->logic-vector val 12))
      (check-eqv? (logic-vector->integer lv)
                  val))

    (test-case "Can convert an 8-bit vector into a signed integer"
      (define val -73)
      (define lv (integer->logic-vector val 8 #t))
      (check-equal? (logic-vector->integer lv)
                    val))

    (test-case "Can convert a 12-bit vector into a signed integer"
      (define val -735)
      (define lv (integer->logic-vector val 12 #t))
      (check-equal? (logic-vector->integer lv)
                    val))))
