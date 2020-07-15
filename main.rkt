#lang racket

(require
  "lib/typechecker.rkt"
  "lib/decorator.rkt"
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
  (define top (typecheck (decorate stx)))
  ; (displayln (syntax->datum top))
  #`(begin
      ; #,(ast->proc top)
      #,top))
