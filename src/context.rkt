#lang racket

(require
  syntax/parse)

(provide
  (all-defined-out))

(define (element-type stx)
  (first (syntax->datum stx)))

(define (element-id stx)
  (second (syntax->datum stx)))

(define (is-context? stx)
  (member (element-type stx)
    '(module
      interface
      component)))

(define (add-to-context? stx)
  (member (element-type stx)
    '(module
      interface
      component
      parameter
      data-port
      composite-port)))

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

(define (decorate ctx stx)
  (syntax-parse stx
    [(rule child ...)
     #:when (symbol? (element-type stx))
     (let* ([children-ctx (if (is-context? stx) (make-context ctx) ctx)]
            [children (for/list ([c (syntax->list #'(child ...))])
                        (decorate children-ctx c))]
            [stx1 #`(rule #,@children)]
            [stx2 (syntax-property stx1 'parent-context ctx)]
            [stx3 (syntax-property stx2 'children-context children-ctx)])
       (when (add-to-context? stx)
         (context-set! ctx (element-id stx) stx3))
       stx3)]

    ; Transform a list of items into a list of decorated items.
    [(item ...)
     #`(#,@(for/list ([i (syntax->list #'(item ...))])
             (decorate ctx i)))]

    ; Fallback: the current syntax object is a simple token.
    [_ stx]))
