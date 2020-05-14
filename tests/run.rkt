
#lang racket

(require
  rackunit/text-ui
  "signal-tests.rkt"
  "compiler-tests.rkt"
  "logic-vector-tests.rkt")

(run-tests signal-tests)
(run-tests compiler-tests)
(run-tests logic-vector-tests)
