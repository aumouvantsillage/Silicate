#lang racket

(require (for-syntax racket/syntax))
(require "signal.rkt")

(provide
    define-interface
    define-component
    interface-ref
    interface-set!)

(begin-for-syntax

  (define (format-ctor-id id)
    (format-id id "make-~a" id))

  (define (interface-to-struct id ports)
    #`(struct #,id
        #,(for/list ([p ports])
            (syntax-case p ()
              ; Keep only the name of each port.
              [(_ pid _ ...) #'pid]))))

  (define (interface-constructor-call stx)
    (syntax-case stx ()
      ; General case: the port uses an interface with arguments
      ; and has an explicit multiplicity.
      [(_ id (iid arg ...) expr)
       (with-syntax ([mk (format-ctor-id #'iid)])
         #'(if (> expr 1)
             (build-vector expr (λ (i) (mk arg ...)))
             (mk arg ...)))]
      ; Special case: arguments and no explicit multiplicity.
      [(mode id (iid arg ...))
       (interface-constructor-call #'(mode id (iid arg ...) 1))]
      ; Special case: no argument and explicit multiplicity.
      [(mode id iid expr)
       (interface-constructor-call #'(mode id (iid) expr))]
      ; Special case: no argument and no explicit multiplicity.
      [(mode id iid)
       (interface-constructor-call #'(mode id (iid) 1))]))

  (define (parameter-names params)
    (for/list ([p params])
      (syntax-case p ()
        [(pid _ ...) #'pid])))

  (define (port-names ports)
    (for/list ([p ports])
      (syntax-case p ()
        [(_ pid _ ...) #'pid])))

  (define (interface-to-constructor id params ports)
    (with-syntax ([mk (format-ctor-id id)])
      #`(define (mk #,@(parameter-names params))
          ; Call the default constructor and initialize each port.
          (#,id #,@(for/list ([p ports])
                          (syntax-case p (in out inout)
                            ; If the port is a plain input or output,
                            ; create an empty box.
                            [(in     _ ...) #'(box #f)]
                            [(inout  _ ...) #'(box #f)]
                            [(out    _ ...) #'(box #f)]
                            [_              (interface-constructor-call p)])))))))

; Syntax of interfaces:
;
; intf ::= (define-interface intf-id (mode port-id type expr?) ...)
; mode ::= in | out | use | flip
; type ::= intf-id | data-type-spec
;
; TODO mode and data-type-spec are ignored
(define-syntax (define-interface stx)
  (syntax-case stx ()
    [(_ id (param ...) (port ...))
     (let ([ports  (syntax->list #'(port ...))]
           [params (syntax->list #'(param ...))])
       #`(begin
           #,(interface-to-struct      #'id ports)
           #,(interface-to-constructor #'id params ports)))]))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ id (param ...) (port ...) body ...)
     (let ([ports  (syntax->list #'(port ...))]
           [params (syntax->list #'(param ...))])
       #`(define (id #,@(parameter-names params) #,@(port-names ports)) body ...))]))

; Get the box that contains a signal from an interface of the current component.
(define-syntax interface-ref*
  (syntax-rules ()
    [(interface-ref* x) x]
    [(interface-ref* a b c ...)
     (let* ([va a]
            [vb b]
            [vba (if (vector? va) (vector-ref va vb) (vb va))])
       (interface-ref* vba c ...))]))

; Get a proxy to a signal from an interface of the current component.
(define-syntax interface-ref
  (syntax-rules ()
    [(interface-ref (s ...) x ...)
     ((lift (λ (s ...) (signal-first (interface-ref x ...)))) s ...)]
    [(interface-ref x ...)
     (signal-proxy (unbox (interface-ref* x ...)))]))

; Assign a signal to a port in the interface of the current component.
; Transforms: (interface-set! a b c d y)
; Into:       (set-box! (d (c (b a)))) y)
(define-syntax-rule (interface-set! x ... y)
  (set-box! (interface-ref* x ...) y))
