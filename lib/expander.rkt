#lang racket

(require
  syntax/parse/define
  "signal.rkt"
  (for-syntax
    (only-in racket empty identity in-syntax)
    racket/syntax
    syntax/parse/define
    (prefix-in stx/ "syntax-classes.rkt")))

(provide
  module
  interface
  component
  design-unit-field-ctor
  constant
  local-signal
  assignment
  literal-expr
  alias
  name-expr
  field-expr
  indexed-expr
  call-expr
  signal-expr
  static-expr
  lift-expr)

; TODO provide

(begin-for-syntax
  (define (channel-ctor-name name)
    (format-id name "make-channel-~a" name))

  (define (instance-ctor-name name)
    (format-id name "make-instance-~a" name))

  (define (accessor-name sname fname)
    (format-id fname "~a-~a" sname fname))

  ; This macro can be used to implement map, filter, or a combination of both,
  ; on a list syntax object.
  ;
  ; It defines a function that parses the elements of a list syntax object.
  ; The body is composed of parse options and clauses supported by syntax-parse.
  ; Elements that do not match any syntax pattern are filtered out.
  (define-simple-macro (define/map-syntax name body ...)
    (define (name lst)
      (filter identity
        (for/list ([i (in-list (or lst empty))])
          (syntax-parse i
            body ... [_ #f])))))

  ; Return the list of ports and signals in the given syntax object.
  (define/map-syntax design-unit-field-syntaxes
    [:stx/data-port      this-syntax]
    [:stx/composite-port this-syntax]
    [:stx/local-signal   this-syntax]
    [:stx/instance       this-syntax])

  ; Return the list of port and signal names in the given syntax object.
  (define/map-syntax design-unit-field-names
    [:stx/data-port      #'name]
    [:stx/composite-port #'name]
    [:stx/local-signal   #'name]
    [:stx/instance       #'name])

  (define/map-syntax design-unit-statements
    [:stx/constant     this-syntax]
    [:stx/local-signal this-syntax]
    [:stx/alias        this-syntax]
    [:stx/assignment   this-syntax])

  (define/map-syntax design-unit-aliases
    [:stx/alias this-syntax])

  ; Return the list of parameter names in the given syntax object.
  (define/map-syntax design-unit-parameter-names
    [:stx/parameter #'name]))

; Generate a module.
(define-simple-macro (module body ...)
  (begin body ...))

; Generate a struct type and a constructor function from an interface.
(define-syntax-parser interface
  [:stx/interface
   #:with ctor-name        (channel-ctor-name #'name)
   #:with (param-name ...) (design-unit-parameter-names (attribute param))
   #:with (field-name ...) (design-unit-field-names     (attribute body))
   #:with (field-stx ...)  (design-unit-field-syntaxes  (attribute body))
   #:with (alias-stx ...)  (design-unit-aliases         (attribute body))
   #`(begin
       (struct name (field-name ...) #:transparent)
       (define (ctor-name param-name ...)
         (name (design-unit-field-ctor field-stx) ...))
       #,@(for/list ([i (in-list (attribute alias-stx))])
            (define/syntax-parse a:stx/alias i)
            ; For each alias, create an accessor in the current interface.
            #`(define (#,(accessor-name #'name #'a.name) x)
                ; Call the original accessor for the aliased field in the target interface...
                (#,(accessor-name #'a.intf-name #'a.name)
                  ; ... with the result of the accessor for the spliced composite port in the current interface.
                  (#,(accessor-name #'name #'a.port-name) x)))))])

; From a component, generate the same output as for an interface,
; and a function with the body of the component.
(define-syntax-parser component
  [:stx/component
   #:with inst-ctor-name   (instance-ctor-name #'name)
   #:with chan-ctor-name   (channel-ctor-name  #'name)
   #:with (param-name ...) (design-unit-parameter-names (attribute param))
   #:with (stmt ...)       (design-unit-statements      (attribute body))
   #`(begin
       (interface name param ... body ...)
       (define (inst-ctor-name param-name ...)
         (define chan (chan-ctor-name param-name ...))
         #,@(for/list ([i (in-list (design-unit-field-names (attribute body)))])
              (define acc (accessor-name #'name i))
              #`(define #,i (#,acc chan)))
         stmt ...
         chan))])

; Data port initialization in a channel constructor.
(define-syntax (design-unit-field-ctor stx)
  (syntax-parse stx
    [(_ :stx/data-port)    #'(box #f)]
    [(_ :stx/local-signal) #'(box #f)]
    [(_ :stx/composite-port)
     #:with m (or (attribute mult) #'1)
     #:with chan-ctor-name (channel-ctor-name #'intf-name)
     #'(let ([ctor (λ (z) (chan-ctor-name arg ...))])
         (if (> m 1)
           (build-vector m ctor)
           (ctor #f)))]
    [(_ :stx/instance)
     #:with m (or (attribute mult) #'1)
     #:with inst-ctor-name (instance-ctor-name #'comp-name)
     #'(let ([ctor (λ (z) (inst-ctor-name arg ...))])
         (if (> m 1)
           (build-vector m ctor)
           (ctor #f)))]))

(define-simple-macro (constant name expr)
  (define name expr))

(define-simple-macro (local-signal name expr)
  (set-box! name expr))

(define-simple-macro (alias name port-name intf-name)
  (define name (field-expr (name-expr port-name) name intf-name)))

; An assignment fills the target port's box with the signal
; from the right-hand side.
(define-simple-macro (assignment target expr)
  (set-box! target expr))

; Expand a literal expression to its value.
(define-simple-macro (literal-expr value)
  value)

; A name expression refers to a variable in the current scope.
(define-simple-macro (name-expr name)
  name)

; After type checking, a field expression contains the name
; of the interface or record type where the field is declared.
; A field expression expands to a field access in a struct instance.
(define-syntax-parser field-expr
  [:stx/field-expr
   #:with acc (accessor-name #'type-name #'field-name)
   #'(acc expr)])

; An indexed expression expands to a chain of vector accesses.
(define-syntax-parser indexed-expr
  [(indexed-expr expr index ... last)
   #'(vector-ref (indexed-expr expr index ...) last)]
  [(indexed-expr expr)
   #'expr])

(define-simple-macro (call-expr fn-name arg ...)
  (fn-name arg ...))

; A signal expression is a wrapper element added by the typechecker
; to identify an expression that refers to a port or local signal
; for reading.
(define-simple-macro (signal-expr expr)
  (signal-proxy (unbox expr)))

(define-simple-macro (static-expr expr)
  (static expr))

; A lift expression is a wrapper element added by the typechecker
; when an expression depends on some signal values.
; name ... is a list of signal names that are needed to compute expr.
(define-syntax-parser lift-expr
  #:literals [signal-expr]
  ; Lift a signal expression. Since the expression returns a signal,
  ; we must lift signal-first to avoid created a signal of signals.
  ; This is typically used when indexed-expr contain signals as indices.
  [(lift-expr binding ...+ (signal-expr expr))
   #'(lift-expr binding ... (signal-first (signal-expr expr)))]
  ; Lift any expression that computes values from values.
  ; expr must not contain elements of type signal-expr.
  [(lift-expr (name sexpr) ...+ expr)
   #'((lift (λ (name ...) expr)) sexpr ...)])
