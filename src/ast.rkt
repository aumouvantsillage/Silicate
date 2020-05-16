#lang racket

(require
  syntax/parse
  silicate/syntax-classes
  silicate/context)

(provide
  element-type
  element-id
  interface-ports
  interface-parameters)

(define (name-ids stx)
  (syntax-parse stx
    [n:name (syntax->datum #'(n.id ...))]))

(define (name-resolve name stx)
  (define ids (name-ids name))
  (define root (context-lookup (parent-context stx) (first ids)))
  (for/fold ([acc root])
            ([id (rest ids)])
    (and acc (context-ref (children-context acc) id))))

(define (interface-ports stx)
  (syntax-parse stx
    [i:interface
     (flatten (for/list ([it (syntax->list #'(i.item ...))])
                (syntax-parse it
                  [p:port it]
                  [p:inline-composite-port
                   (interface-ports (name-resolve (attribute p.name) it))]
                  [_ '()])))]))

(define (interface-parameters stx)
  (syntax-parse stx
    [i:interface
     (filter (syntax-parser [p:parameter #'p] [_ #f])
             (syntax->list #'(i.item ...)))]))
