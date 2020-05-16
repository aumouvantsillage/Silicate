#lang racket

(require
  (for-syntax
    racket/syntax
    syntax/parse
    silicate/syntax-classes
    silicate/ast))

(provide
  (all-defined-out))

(define-syntax (module stx)
  (syntax-parse stx
    [m:module
     #'(begin m.item ...)]))
  ; (module* id #f item ...))

(define-for-syntax (channel-struct-id id)
  (format-id id "~a:channel" id))

(define-for-syntax (channel-constructor-id id)
  (format-id id "make-~a" (channel-struct-id id)))

(define-syntax (interface stx)
  (syntax-parse stx
    [i:interface
     #:with (pt:port ...)      (interface-ports stx)
     #:with (pr:parameter ...) (interface-parameters stx)
     #`(begin
         (struct #,(channel-struct-id (attribute i.id)) (pt.id ...))
         (define (#,(channel-constructor-id (attribute i.id)) pr.id ...) (void)))]))

(define-syntax-rule (component id (item ...) (stmt ...))
  (define (id) (void)))

#|
(define (named-element->symbol elt)
  (string->symbol (string-join (map symbol->string (sil:name-ids (sil:named-element-fully-qualified-name elt))) "/")))

(define (channel-constructor-id elt)
  (format-symbol "make-~a" (channel-struct-id elt)))

(define (component-function-id elt)
  (format-symbol "~a:component" (named-element->symbol elt)))

(define (model->racket elt)
    [(sil:component stx parent children name items statements)
     #`(begin
         ; Process this component like an interface.
         #,(model->racket (sil:interface stx parent children name items))
         ; Then generate a function with the component's body.
         (define #,(component-function-id elt) $ports
           #,@(map model->racket statements)))]

    [(sil:interface _ _ _ _ _)
     (let ([sid (channel-struct-id elt)])
       #`(begin
           ; Create a struct type with a field for each port.
           (struct #,sid #,(map named-element->symbol (sil:interface-ports elt)))
           ; Create a constructur with an argument for each parameter.
           (define (#,(channel-constructor-id elt) #,@(map named-element->symbol (sil:interface-parameters elt)))
             ; Call the default constructor and initialize each port.
             (#,sid #,@(for/list ([p (sil:interface-ports elt)])
                         (match p
                           ; For a data port, generate an empty box.
                           [(sil:data-port _ _ _ _ _ _)
                            #'(box #f)]
                           ; For a composite port, generate a call to the constructor
                           ; for the struct that was generated from this port's interface.
                           [(sil:composite-port _ _ _ _ mult _ intf-name args)
                            (let ([intf (sil:model-element-lookup-in-parent elt intf-name)])
                              #`(let ([ctor (λ (i) (#,(channel-constructor-id intf) #,@(map model->racket args)))]
                                      [mult #,(model->racket mult)])
                                  ; If the multiplicity of this port is more than 1, build a vector where
                                  ; each element is the result of the constructor for the target interface.
                                  (if (> mult 1)
                                      (build-vector mult ctor)
                                      (ctor #f))))]))))))]

    [(sil:port-assignment _ _ _ target expr)
     #`(assign #,(for/list ([p (sil:indexed-name-parts target)])
                   (match p
                     [(? symbol?) p]
                     [(sil:index _ _ _ exprs) (map model->racket exprs)])))]

    [(sil:name _ _ _ _)
     ; Replace a name with the symbol that represents the target element.
     (named-element->symbol (sil:model-element-lookup-in-parent elt elt))]

    [_ elt])
|#
