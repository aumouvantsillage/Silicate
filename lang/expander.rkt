#lang racket

(require silicate)

(provide
  (all-from-out silicate)
  (rename-out [silicate-module-begin #%module-begin]))

(define-syntax-rule (silicate-module-begin form ...)
  (#%module-begin
    (begin-silicate form ...)))