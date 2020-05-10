#lang racket

(require
    (for-syntax
      racket
      racket/syntax
      silicate/metamodel
      silicate/compiler))

(provide
  (except-out (all-defined-out) module-begin)
  (rename-out [module-begin #%module-begin]))

(begin-for-syntax
  (define silicate-ns (module->namespace 'silicate/metamodel))

  (define (syntax->model parent stx)
    (syntax-case stx (composite-port inline-composite-port name indexed-name)
      ; Set default values for missing elements in composite-port
      [(composite-port id mode iname args) (symbol? (syntax->datum #'mode))
       (syntax->model parent #'(composite-port id 1 mode iname args))]
      [(composite-port id mode iname)
       (syntax->model parent #'(composite-port id mode iname empty))]
      [(inline-composite-port mode iname)
       (syntax->model parent #'(inline-composite-port mode iname empty))]

      ; Gather names in a single list in name productions
      [(name id ...)
       (sil-name stx parent empty (syntax->datum #'(id ...)))]
      [(indexed-name id ...)
       (sil-indexed-name stx parent empty (syntax->datum #'(id ...)))]

      ; Default case: transform a production into a struct instance.
      ; Build the struct constructor name from the production rule name.
      ; Construct children model elements recursively.
      ; Attach the new element as a parent to its children elements.
      [(rule child ...) (symbol? (syntax->datum #'rule))
       (let* ([ctor-id (format-id #'rule "sil-~a" #'rule)]
              [args (for/list ([c (syntax->list #'(child ...))])
                      (syntax->model #f c))]
              [children (filter sil-model-element? (flatten args))]
              [elt (apply (eval ctor-id silicate-ns) stx parent children args)])
         (for ([c children])
           (set-sil-model-element-parent! c elt))
         elt)]

      ; Transform a list of items into a list of transformed items.
      [(item ...)
       (for/list ([i (syntax->list #'(item ...))])
         (syntax->model parent i))]

      ; Fallback: the item is a simple token.
      [item
       (syntax->datum #'item)])))

(define-syntax (main stx)
  (syntax-case stx ()
    [(_ s)
     #`(quote #,(model->racket (syntax->model #f #'s)))]))

(define-syntax-rule (module-begin stx)
  (#%module-begin
    (writeln (main stx))))
