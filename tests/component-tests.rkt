#lang racket

(require
  rackunit
  silicate/component)

(provide component-tests)

(define component-tests
  (test-suite "Components"
    (test-case "Interface instance has boxes for simple ports"
      (interface I () ([a in 'integer] [b out 'integer]))
      (define an-I (make-I))
      (check-pred box? (I-a an-I))
      (check-pred box? (I-b an-I)))))
