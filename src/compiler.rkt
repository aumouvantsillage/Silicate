#lang racket

(require
  syntax/parse/define
  (for-syntax
    (except-in racket module interface)
    racket/syntax
    syntax/parse
    silicate/syntax-classes
    silicate/ast))

(provide
  module
  interface
  component
  name)

; FIXME Should we generate Racket modules?
(define-syntax-parser module
  [m:module
   #'(begin m.item ...)])
  ; (module* id #f item ...))

; Convert the given interface id into a struct id
; for channels that instanciate that interface.
(define-for-syntax (channel-struct-id id)
  (format-id id "~a:channel" id))

; Convert the given interface id into a constructor function id
; for channels that instanciate that interface.
(define-for-syntax (channel-constructor-id id)
  (format-id id "make-~a" (channel-struct-id id)))

; Convert an interface into a channel struct type and
; a channel constructor function.
(define-syntax-parser interface
  [i:interface
   #:with sid (channel-struct-id (attribute i.id))
   #:with cid (channel-constructor-id (attribute i.id))
   #:with (pt:port ...) (interface-ports this-syntax)
   #:with (pr:parameter ...) (interface-parameters this-syntax)
   #`(begin
       (struct sid (pt.id ...))
       (define (cid pr.id ...)
         (sid #,@(for/list ([p (syntax->list #'(pt ...))])
                   (syntax-parse p
                     [d:data-port #'(box #f)]
                     [c:composite-port
                      #:with j:interface (composite-port-interface #'c)
                      #:with jcid (channel-constructor-id (attribute j.id))
                      #:with mult (or (attribute c.mult) #'1)
                      #`(let ([ctor (λ (z) (jcid c.arg ...))])
                          (if (> mult 1)
                            (build-vector mult ctor)
                            (ctor #f)))])))))])

; TODO Convert a component into a channel struct type,
; a channel constructor function, and an implementation function.
(define-syntax-rule (component id (item ...) (stmt ...))
  (define (id) (void)))

; Convert a name into a Racket expression.
; FIXME This works only if the name contains only one id
(define-syntax-parser name
    [n:name
     (datum->syntax this-syntax (last (syntax->list #'(n.id ...))))])
