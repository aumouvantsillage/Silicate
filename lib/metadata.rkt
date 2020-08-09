#lang racket

(require
  syntax/parse
  syntax/parse/define
  syntax/id-table
  (prefix-in stx/ "syntax-classes.rkt")
  "scope.rkt")

(provide (all-defined-out))

; A node has a reference to its original syntax object.
(struct metadata (stx) #:transparent)

(struct design-unit metadata (local-scope))

; TODO local scope should include only ports,
(define (make-local-scope lst [sc (make-immutable-free-id-table)])
  (for/fold ([acc sc])
            ([i (in-list lst)])
    (syntax-parse i
      [:stx/named-elt
       (define meta (syntax-property i 'meta))
       (unless meta
         (raise-syntax-error #f "No metadata attached to name" #'name))
       (dict-set acc #'name meta)]
      [_ acc])))

(define (make-design-unit-local-scope stx)
  (define/syntax-parse :stx/design-unit stx)
  (make-local-scope (attribute param) (make-local-scope (attribute body))))

(define (design-unit-ref unit name)
  (dict-ref (design-unit-local-scope unit) name
    (λ () (raise-syntax-error #f "No element with this name" name))))

(struct interface design-unit ())

(define (make-interface stx)
  (interface stx (make-design-unit-local-scope stx)))

(struct component design-unit ())

(define (make-component stx)
  (component stx (make-design-unit-local-scope stx)))

(struct signal metadata ())
(struct data-port signal (mode))
(struct local-signal signal ())

(struct parameter metadata ())
(struct composite-port metadata (flip?))
(struct constant metadata ())
(struct instance metadata ())
