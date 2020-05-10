
#lang racket

(require
  rackunit/text-ui
  "signal-tests.rkt"
  "component-tests.rkt"
  "logic-vector-tests.rkt")

(run-tests signal-tests)
(run-tests component-tests)
(run-tests logic-vector-tests)
