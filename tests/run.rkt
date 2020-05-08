
#lang racket

(require rackunit/text-ui)

(require "signal-tests.rkt")
(require "component-tests.rkt")
(require "logic-vector-tests.rkt")

(run-tests signal-tests)
(run-tests component-tests)
(run-tests logic-vector-tests)
