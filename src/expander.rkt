#lang racket

(require
    silicate/compiler
    (for-syntax
      syntax/parse
      silicate/context))

(provide
  (except-out (all-defined-out) module-begin)
  (rename-out [module-begin #%module-begin])
  (all-from-out silicate/compiler))

(define-syntax (begin-with-context stx)
  (syntax-parse stx
    [(_ item ...)
     #`(begin #,@(decorate (make-context) #'(item ...)))]))

(define-syntax-rule (module-begin form)
  (#%module-begin (begin-with-context form)))
