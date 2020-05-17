#lang racket

(require
  syntax/parse/define
  silicate/context-wrapper
  silicate/compiler)

(provide
  (except-out (all-defined-out) module-begin)
  (rename-out [module-begin #%module-begin])
  (all-from-out silicate/compiler))

(define-syntax-rule (module-begin form)
  (#%module-begin (begin-with-context form)))
