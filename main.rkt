#lang racket

(require
  (for-syntax
    "lib/typechecker.rkt"
    "lib/ast-mapper.rkt")
  "lib/expander.rkt"
  "lib/signal.rkt"
  "lib/std.rkt")

(provide
  begin-silicate
  (all-from-out
    "lib/expander.rkt"
    "lib/signal.rkt"
    "lib/std.rkt"))

(define-syntax (begin-silicate stx)
  (define top (typecheck (syntax->ast stx)))
  #`(begin
      #,(ast->proc top)
      #,@(ast->syntax top)))
