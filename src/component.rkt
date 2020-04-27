#lang racket

(require (for-syntax racket/syntax))
(require "signal.rkt")

(provide
    define-interface
    interface-ref
    interface-set!)

(define-for-syntax (format-ctor-id id)
  (format-id id "make-~a" id))

(define-for-syntax (interface-to-struct id fields)
  #`(struct #,id
      #,(for/list ([f fields])
          (syntax-case f ()
            ; Keep only the name of each field.
            [(_ fid _ ...) #'fid]))))

(define-for-syntax (interface-constructor-call stx)
  (syntax-case stx ()
    ; General case: the field uses an interface with arguments
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

(define-for-syntax (interface-to-constructor id params fields)
  (with-syntax ([mk (format-ctor-id id)])
    #`(define (mk #,@(for/list ([p params])
                       (syntax-case p ()
                         [(pid _ ...) #'pid])))
        ; Call the default constructor and initialize each field.
        (#,id #,@(for/list ([f fields])
                        (syntax-case f (in out inout)
                          ; If the field is a plain input or output,
                          ; create an empty box.
                          [(in     _ ...) #'(box #f)]
                          [(inout  _ ...) #'(box #f)]
                          [(out    _ ...) #'(box #f)]
                          [_              (interface-constructor-call f)]))))))

; Syntax of interfaces:
;
; intf ::= (define-interface intf-id (mode field-id type expr?) ...)
; mode ::= in | out | use | flip
; type ::= intf-id | data-type-spec
;
; TODO mode and data-type-spec are ignored
(define-syntax (define-interface stx)
  (syntax-case stx ()
    [(_ id (param ...) (field ...))
     (let ([fields (syntax->list #'(field ...))]
           [params (syntax->list #'(param ...))])
       #`(begin
           #,(interface-to-struct      #'id fields)
           #,(interface-to-constructor #'id params fields)))]))

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

; Assign a signal to a field in the interface of the current component.
; Transforms: (interface-set! a b c d y)
; Into:       (set-box! (d (c (b a)))) y)
(define-syntax-rule (interface-set! x ... y)
  (set-box! (interface-ref* x ...) y))
