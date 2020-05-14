#lang racket

(require
  (for-syntax
    racket
    racket/syntax
    syntax/parse)
  silicate/signal)

(provide
    interface
    component
    port-ref
    port-set!)

(begin-for-syntax

  ; Normalize the syntax object that represents a port, adding
  ; default multiplicity and arguments if they are missing.
  (define (port-normalize port)
    (syntax-parse port
      ; General case: the port uses an interface with arguments
      ; and has an explicit multiplicity.
      [(id expr mode (iid arg ...)) port]
      ; Special case: arguments and no explicit multiplicity.
      [(id mode (iid arg ...)) #'(id 1 mode (iid arg ...))]
      ; Special case: no argument and explicit multiplicity.
      [(id expr mode iid) #'(id expr mode (iid))]
      ; Special case: no argument and no explicit multiplicity.
      [(id mode iid) #'(id 1 mode (iid))]
      ; Invalid case.
      [_ (error "Invalid port definition" port)]))

  ; Returns the list of port or parameter names.
  (define (interface-item-names items)
    (for/list ([p items])
      (syntax-parse p
        [(pid _ ...) #'pid])))

  ; Create a struct type for an interface with the given id and ports.
  (define (interface-to-struct id ports)
    #`(struct #,id #,(interface-item-names ports)))

  ; Format the id of a constructor function for an interface.
  (define (format-ctor-id id)
    (format-id id "make-~a" id))

  ; Call the constructor of the interface for a composite port.
  ; The given port must be a syntax object from normalize-port.
  (define (interface-constructor-call port)
    (syntax-parse port
      [(id expr mode (iid arg ...))
       (with-syntax ([mk (format-ctor-id #'iid)])
         #'(if (> expr 1)
             (build-vector expr (λ (i) (mk arg ...)))
             (mk arg ...)))]))

  ; Create a constructor for the interface with the given id,
  ; parameters and ports.
  (define (interface-to-constructor id params ports)
    (with-syntax ([mk (format-ctor-id id)])
      #`(define (mk #,@(interface-item-names params))
          ; Call the default constructor and initialize each port.
          (#,id #,@(for/list ([p ports])
                          (syntax-parse p
                            #:datum-literals [in out]
                            ; If the port is a plain input or output,
                            ; create an empty box.
                            [(_ _ in  _ ...) #'(box #f)]
                            [(_ _ out _ ...) #'(box #f)]
                            [_                 (interface-constructor-call p)])))))))

; Create a new interface.
(define-syntax (interface stx)
  (syntax-parse stx
    [(_ id (param ...) (port ...))
     (let ([ports  (map port-normalize (syntax->list #'(port ...)))]
           [params (syntax->list #'(param ...))])
       #`(begin
           #,(interface-to-struct      #'id ports)
           #,(interface-to-constructor #'id params ports)))]))

; Create a new component.
(define-syntax (component stx)
  (syntax-parse stx
    [(_ id (param ...) (port ...) body ...)
     (let ([ports  (map port-normalize (syntax->list #'(port ...)))]
           [params (syntax->list #'(param ...))])
       #`(define (id #,@(interface-item-names params) #,@(interface-item-names ports)) body ...))]))

; Get the box that contains a signal from an interface of the current component.
(define-syntax port-ref*
  (syntax-rules ()
    [(port-ref* x) x]
    [(port-ref* a b c ...)
     (let* ([va a]
            [vb b]
            [vba (if (vector? va) (vector-ref va vb) (vb va))])
       (port-ref* vba c ...))]))

; Get a proxy to a signal from an interface of the current component.
(define-syntax port-ref
  (syntax-rules ()
    [(port-ref (s ...) x ...)
     ((lift (λ (s ...) (signal-first (port-ref x ...)))) s ...)]
    [(port-ref x ...)
     (signal-proxy (unbox (port-ref* x ...)))]))

; Assign a signal to a port in the interface of the current component.
(define-syntax-rule (port-set! x ... y)
  (set-box! (port-ref* x ...) y))
