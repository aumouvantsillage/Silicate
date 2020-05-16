#lang racket

(require
  syntax/parse
  silicate/syntax-classes)

(provide
  make-context
  context-resolve
  decorate)

(struct context (parent table))

(define (make-context [ctx #f])
  (context ctx (make-hash)))

(define (context-set! ctx id val)
  (hash-set! (context-table ctx) id val))

(define (context-ref ctx id)
  (define table (context-table ctx))
  (and (hash-has-key? table id)
       (hash-ref table id)))

(define (context-lookup ctx id)
  (match ctx
    [(context parent table)
     (or (context-ref ctx id)
       (and parent (context-lookup parent id)))]))

(define (parent-context stx)
  (syntax-property stx 'parent-context))

(define (children-context stx)
  (syntax-property stx 'children-context))

(define (make-children-context ctx stx)
  (syntax-parse stx
    [c:create-context (make-context ctx)]
    [_                ctx]))

(define element-id
  (syntax-parser
    [c:add-to-context (syntax->datum (attribute c.id))]
    [_                #f]))

(define (decorate ctx stx)
  (syntax-parse stx
    [(rule child ...)
     #:when (symbol? (syntax->datum #'rule))
     (let* ([id (element-id stx)]
            [children-ctx (make-children-context ctx stx)]
            [children (for/list ([c (syntax->list #'(child ...))])
                        (decorate children-ctx c))]
            [stx1 #`(rule #,@children)]
            [stx2 (syntax-property stx1 'parent-context ctx)]
            [stx3 (syntax-property stx2 'children-context children-ctx)])
       (when id
         (context-set! ctx id stx3))
       stx3)]

    ; Transform a list of items into a list of decorated items.
    [(item ...)
     #`(#,@(for/list ([i (syntax->list #'(item ...))])
             (decorate ctx i)))]

    ; Fallback: the current syntax object is a simple token.
    [_ stx]))

(define (context-resolve ids stx)
  (define root (context-lookup (parent-context stx) (first ids)))
  (for/fold ([acc root])
            ([id (rest ids)])
    (and acc (context-ref (children-context acc) id))))
