#lang racket

(provide (all-defined-out))

(struct sil-model-element (stx [parent #:mutable] children))

(struct sil-named-element sil-model-element (name))

(define (sil-model-element-child-with-name elt name)
  (let sil-model-element-child-with-ids ([current elt] [ids (sil-name-ids name)])
         (define (found? e)
           (and (sil-named-element? e) (eq? (sil-named-element-name e) (first ids))))
         (define child (findf found? (sil-model-element-children current)))
         (cond [(empty? (rest ids)) child]
               [child (sil-model-element-child-with-ids child (rest ids))]
               [else #f])))

(define (sil-model-element-lookup elt name)
  (or
    ; Find the target in the current element's children.
    (sil-model-element-child-with-name elt name)
    ; If not found, look up the target in the parent.
    (sil-model-element-lookup-in-parent elt name)
    (raise-user-error "Symbol not found" (sil-model-element-stx name))))

(define (sil-model-element-lookup-in-parent elt name)
  ; If elt has a parent, look up the target in the parent.
  (let ([parent (sil-model-element-parent elt)])
    (and parent (sil-model-element-lookup parent name))))

(define (sil-named-element-fully-qualified-name elt)
  (let sil-named-element-ids ([current elt] [ids empty])
    (cond [(sil-named-element? current)
           (sil-named-element-ids (sil-model-element-parent current)
                                  (cons (sil-named-element-name current) ids))]
          [current
           (sil-named-element-ids (sil-model-element-parent current) ids)]
          [else
           (sil-name (sil-model-element-stx elt) elt empty ids)])))


(struct sil-module sil-named-element (items))

(struct sil-interface sil-named-element (items))

; Return all parameters in the given interface, preserving their declaration order.
(define (sil-interface-parameters intf)
  (filter sil-parameter? (sil-interface-items intf)))

; Return all ports in the given interface, preserving their declaration order.
; Inline composite ports are inlined.
(define (sil-interface-ports intf)
  (flatten
    (for/list ([p (sil-interface-items intf)] #:unless (sil-parameter? p))
      (if (sil-inline-composite-port? p)
        (sil-interface-ports (sil-model-element-lookup-in-parent intf (sil-inline-composite-port-interface-name p)))
        p))))

; TODO statements
(struct sil-component sil-interface (statements))

(struct sil-parameter sil-named-element (type))

(struct sil-data-port sil-named-element (mode type))

(struct sil-composite-port sil-named-element (multiplicity mode interface-name args))

(struct sil-inline-composite-port sil-model-element (mode interface-name args))

(struct sil-port-assignment sil-model-element (target expr))

(struct sil-name sil-model-element (ids))

(struct sil-indexed-name sil-model-element (parts))

(struct sil-index sil-model-element (exprs))
