#lang racket

(provide (all-defined-out))

(struct model-element (stx [parent #:mutable] children))

(struct named-element model-element (name))

(define (model-element-child-with-name elt name)
  (let model-element-child-with-ids ([current elt] [ids (name-ids name)])
         (define (found? e)
           (and (named-element? e) (eq? (named-element-name e) (first ids))))
         (define child (findf found? (model-element-children current)))
         (cond [(empty? (rest ids)) child]
               [child (model-element-child-with-ids child (rest ids))]
               [else #f])))

(define (model-element-lookup elt name)
  (or
    ; Find the target in the current element's children.
    (model-element-child-with-name elt name)
    ; If not found, look up the target in the parent.
    (model-element-lookup-in-parent elt name)
    (raise-user-error "Symbol not found" (model-element-stx name))))

(define (model-element-lookup-in-parent elt name)
  ; If elt has a parent, look up the target in the parent.
  (let ([parent (model-element-parent elt)])
    (and parent (model-element-lookup parent name))))

(define (named-element-fully-qualified-name elt)
  (let named-element-ids ([current elt] [ids empty])
    (cond [(named-element? current)
           (named-element-ids (model-element-parent current)
                              (cons (named-element-name current) ids))]
          [current
           (named-element-ids (model-element-parent current) ids)]
          [else
           (name (model-element-stx elt) elt empty ids)])))


(struct module named-element (items))

(struct interface named-element (items))

; Return all parameters in the given interface, preserving their declaration order.
(define (interface-parameters intf)
  (filter parameter? (interface-items intf)))

; Return all ports in the given interface, preserving their declaration order.
; Inline composite ports are inlined.
(define (interface-ports intf)
  (flatten
    (for/list ([p (interface-items intf)] #:unless (parameter? p))
      (if (inline-composite-port? p)
        (interface-ports (model-element-lookup-in-parent intf (inline-composite-port-interface-name p)))
        p))))

(struct component interface (statements))

(struct parameter named-element (type))

(struct data-port named-element (mode type))

(struct composite-port named-element (multiplicity mode interface-name args))

(struct inline-composite-port model-element (mode interface-name args))

; TODO other statements
(struct port-assignment model-element (target expr))

(struct name model-element (ids))

(struct indexed-name model-element (parts))

(struct index model-element (exprs))
