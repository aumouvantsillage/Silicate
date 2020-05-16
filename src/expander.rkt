#lang racket

(require
    syntax/parse/define
    silicate/compiler
    (for-syntax
      syntax/parse
      silicate/context))

(provide
  (except-out (all-defined-out) module-begin)
  (rename-out [module-begin #%module-begin])
  (all-from-out silicate/compiler))

(define-syntax-parser begin-with-context
    [(_ item ...)
     #`(begin #,@(decorate (make-context) #'(item ...)))])

(define-syntax-rule (module-begin form)
  (#%module-begin (begin-with-context form)))
