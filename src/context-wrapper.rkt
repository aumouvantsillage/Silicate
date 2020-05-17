#lang racket

(require
  syntax/parse/define
  (for-syntax
    silicate/context))

(provide
  begin-with-context)

(define-syntax-parser begin-with-context
    [(_ item ...)
     #`(begin #,@(decorate (make-context) #'(item ...)))])
