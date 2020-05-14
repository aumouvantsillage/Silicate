#lang racket

(require
  syntax/parse
  silicate/syntax-classes
  silicate/context)

(provide
  element-type
  element-id
  interface-ports)

(define (interface-ports stx)
  (syntax-parse stx
    #:datum-literals [sil-interface sil-component]
    [(sil-interface _ (item ...))
     (flatten (for/list ([it (syntax->list #'(item ...))])
                (syntax-parse it
                  #:datum-literals [sil-data-port sil-composite-port sil-inline-composite-port sil-name]
                  [(sil-data-port      _ ...) it]
                  [(sil-composite-port _ ...) it]
                  [(sil-inline-composite-port _ (sil-name id ...) _ ...)
                   (interface-ports (resolve it (syntax->datum #'(id ...))))]
                  [_ '()])))]))
