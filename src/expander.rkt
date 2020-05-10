#lang racket

(require
    (for-syntax
      racket
      racket/syntax
      (prefix-in sil: silicate/metamodel)
      silicate/compiler))

(provide
  (except-out (all-defined-out) module-begin)
  (rename-out [module-begin #%module-begin]))

(begin-for-syntax
  (define silicate-ns (module->namespace 'silicate/metamodel))

  (define (syntax->model stx)
    (or (syntax-property stx 'model)
        (let ([lst (syntax->list stx)])
          (if lst
            (map syntax->model lst)
            (syntax->datum stx)))))

  ; Convert a syntax object to a model.
  ;
  ; * parent is the parent model element of the new model.
  ; * stx is the syntax object to convert.
  ;
  ; Returns:
  ; * a decorated syntax object with the 'model property
  (define (decorate parent stx)
    (syntax-case stx (composite-port inline-composite-port name indexed-name)
      ; Set default values for missing elements in composite-port
      [(composite-port id mode iname args) (symbol? (syntax->datum #'mode))
       (decorate parent #'(composite-port id 1 mode iname args))]
      [(composite-port id mode iname)
       (decorate parent #'(composite-port id mode iname empty))]
      [(inline-composite-port mode iname)
       (decorate parent #'(inline-composite-port mode iname empty))]

      ; Gather names in a single list in name productions
      [(name id ...)
       (syntax-property stx 'model (sil:name stx parent empty (syntax->datum #'(id ...))))]

      [(indexed-name id ...)
       (syntax-property stx 'model (sil:indexed-name stx parent empty (syntax->datum #'(id ...))))]

      ; Default case: transform a production into a struct instance.
      ; Build the struct constructor name from the production rule name.
      ; Construct children model elements recursively.
      ; Attach the new element as a parent to its children elements.
      [(rule child ...) (symbol? (syntax->datum #'rule))
       (let* ([children (for/list ([c (syntax->list #'(child ...))])
                          (decorate #f c))]
              [children-model (map syntax->model children)]
              [children-elts (filter sil:model-element? (flatten children-model))]
              [ctor (eval (syntax->datum #'rule) silicate-ns)]
              [model (apply ctor stx parent children-elts children-model)])
         ; Set the parent property on children elements.
         (for ([c children-elts])
           (sil:set-model-element-parent! c model))
         (syntax-property #`(rule #,@children) 'model model))]

      ; Transform a list of items into lists of transformed items.
      [(item ...)
       #`(#,@(for/list ([i (syntax->list #'(item ...))])
               (decorate parent i)))]

      ; Fallback: the item is a simple token.
      [_ stx])))

(define-syntax (main-debug stx)
  (syntax-case stx ()
    [(_ s)
     #`(writeln (quote #,(model->racket (syntax->model (decorate #f #'s)))))]))

(define-syntax (main stx)
  (syntax-case stx ()
    [(_ s)
     (model->racket (syntax->model (decorate #f #'s)))]))

(define-syntax-rule (module-begin stx)
  (#%module-begin
    ; (main-debug stx)
    (main stx)))
