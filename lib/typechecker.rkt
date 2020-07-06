#lang racket

(require (prefix-in ast: "ast.rkt"))

(provide
  (contract-out
    [typecheck (-> ast:node? ast:node?)]))

; TODO (field-expr expr name) -> (field-expr expr name type)

(define (typecheck ast)
  ast)
