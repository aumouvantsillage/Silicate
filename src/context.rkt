#lang racket

(require
  syntax/parse
  silicate/syntax-classes)

(provide
  make-context
  context-resolve
  decorate)

; A context has a parent context (#f for the root context)
; and a hash table that maps ids to syntax objects.
(struct context (parent table))

; Create a new empty context with the given parent.
(define (make-context [ctx #f])
  (context ctx (make-hash)))

; Add an entry to the current context.
(define (context-set! ctx id stx)
  (hash-set! (context-table ctx) id stx))

; Return the syntax object at the given id in the given context.
; Return #f if no matching entry was found.
(define (context-ref ctx id)
  (define table (context-table ctx))
  (and (hash-has-key? table id)
       (hash-ref table id)))

; Return the syntax object at the given id in the chain of parent contexts
; starting from ctx. Return #f if no matching entry was found.
(define (context-lookup ctx id)
  (match ctx
    [(context parent table)
     (or (context-ref ctx id)
       (and parent (context-lookup parent id)))]))

; Return the parent context of the given syntax object.
(define (parent-context stx)
  (syntax-property stx 'parent-context))

; Return the children context of the given syntax object.
(define (children-context stx)
  (syntax-property stx 'children-context))

; Return the syntax object at the fully qualified name represented
; by the given list of ids.
; 1. The first id of the list is looked up in the chain of parent contexts
;    starting from the parent context of stx.
; 2. The rest of the list is looked in the chain of children contexts
;    starting from the children context of the syntax object found in step 1.
(define (context-resolve ids stx)
  (define root (context-lookup (parent-context stx) (first ids)))
  (for/fold ([acc root])
            ([id (rest ids)])
    (and acc (context-ref (children-context acc) id))))

; Creates a new children context for the syntax object stx if it
; belongs to the create-context class.
(define (make-children-context ctx stx)
  (syntax-parse stx
    [c:create-context (make-context ctx)]
    [_                ctx]))

; Return the id of a syntax object if it belongs to the add-to-context class.
(define (element-id stx)
  (syntax-parse stx
    [c:add-to-context (syntax->datum (attribute c.id))]
    [_                #f]))

; Decorate the given syntax object with syntax properties parent-context and
; children-context. Use ctx as the parent context.
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
