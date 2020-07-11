#lang racket

(require
  (prefix-in ast- "ast.rkt")
  "scope.rkt")

(provide
  (contract-out
    [typecheck (-> ast-node? ast-node?)]))

; TODO (field-expr expr name) -> (field-expr expr name type)
; TODO signal access expr     -> (signal-expr ...)
; TODO lift-expr

(define (typecheck n)
  (match n
    [(ast-module stx body)
     (ast-module stx (map typecheck body))]

    [(ast-interface stx name params body _)
     (ast-make-design-unit ast-interface stx name
                           (map typecheck params)
                           (map typecheck body))]

    [(ast-component stx name params body _)
     (ast-make-design-unit ast-component stx name
                           (map typecheck params)
                           (map typecheck body))]

    [(ast-assignment stx target expr)
     (typecheck-assignment stx (typecheck target) (typecheck expr))]

    [(ast-field-expr stx expr name _)
     (typecheck-field-expr stx (typecheck expr) (typecheck name))]

    [(ast-indexed-expr stx expr indices)
     (typecheck-indexed-expr stx (typecheck expr) (map typecheck indices))]

    [_ n]))

(define (typecheck-assignment stx target expr)
  ; The target expression must refer to a data port.
  ; TODO support other targets such as local signals.
  (unless (ast-data-port? (ast-resolve target))
    (raise-syntax-error #f "Invalid assignment target" target))

  (ast-assignment stx target
    ; If the expression refers to a data port, wrap it in a signal-expr.
    ; TODO support local signals.
    (if (ast-data-port? (ast-resolve expr))
      (ast-signal-expr (ast-node-stx expr) expr)
      expr)))

(define (typecheck-field-expr stx expr name)
  (match (ast-resolve expr)
    [(ast-composite-port _ _ _ _ intf-name _)
     ; Check that a port with that name exists in the interface.
     (ast-design-unit-lookin (lookup intf-name ast-interface?) name)
     ; Return a new field-expr with an explicit interface name.
     (ast-field-expr stx expr name intf-name)]

    ; TODO support local signals.

    [_ (raise-syntax-error #f "Expression not suitable for field access" stx)]))

(define (typecheck-indexed-expr stx expr indices)
  (unless (ast-composite-port? (ast-resolve expr))
    (raise-syntax-error #f "Expression not suitable for indexing" stx))
  (ast-indexed-expr stx expr indices))
