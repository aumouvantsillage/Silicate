
#lang racket

(require
  rackunit/text-ui
  "signal-tests.rkt"
  "expander-tests.rkt"
  "typechecker-tests.rkt")

(run-tests signal-tests)
(run-tests expander-tests)
(run-tests typechecker-tests)
