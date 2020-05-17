#lang racket

(require
  syntax/parse
  silicate/syntax-classes
  silicate/context)

(provide
  interface-ports
  interface-parameters
  composite-port-interface)

; Return the list of ids in a name.
(define (name-ids stx)
  (syntax-parse stx
    [n:name (syntax->datum #'(n.id ...))]))

; Return the syntax object corresponding to the given name.
(define (name-resolve name)
  (context-resolve name (name-ids name)))

; Return the list of ports in the given interface.
; This function will collect all ports in inline composite ports.
(define (interface-ports stx)
  (syntax-parse stx
    [i:interface-or-component
     (flatten (for/list ([it (syntax->list #'(i.item ...))])
                (syntax-parse it
                  [p:port it]
                  [p:inline-composite-port
                   (interface-ports (name-resolve (attribute p.type)))]
                  [_ '()])))]))

; Return the list of parameters in the given interface.
(define (interface-parameters stx)
  (syntax-parse stx
    [i:interface-or-component
     (filter (syntax-parser [p:parameter #t] [_ #f])
             (syntax->list #'(i.item ...)))]))

; Return the target interface of an inline composite port.
(define (composite-port-interface stx)
  (syntax-parse stx
    [p:composite-port
     (name-resolve (attribute p.type))]))
