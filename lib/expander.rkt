#lang racket

(require
  syntax/parse/define
  (for-syntax
    racket
    racket/syntax
    syntax/parse/define))

(provide
  module
  interface
  data-port
  composite-port
  component
  assignment
  name-expr
  field-expr
  indexed-expr
  signal-expr)

; Expand a Silicate syntax object after typechecking.
;
; Inline composite ports have been inlined.
; Port references contain the name of the interface.
;
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
    (define (name stx-lst)
      (filter identity
        (for/list ([i (in-syntax stx-lst)])
          (syntax-parse i
            body ... [_ #f])))))

  ; Return the list of data and composite ports in the given syntax object.
  (define/map-syntax ports
    #:datum-literals [data-port composite-port]
    [((~or* data-port composite-port) _ ...) this-syntax])

  ; Return the list of port names in the given syntax object.
  (define/map-syntax port-names
    #:datum-literals [data-port composite-port]
    [((~or* data-port composite-port) name _ ...) #'name])

  ; Return the list of parameter names in the given syntax object.
  (define/map-syntax parameter-names
    #:datum-literals [parameter]
    [(parameter name _ ...) #'name]))

; Generate a module.
(define-simple-macro (module body ...)
  (begin body ...))

; Generate a struct type and a constructor function from an interface.
(define-syntax-parser interface
  [(interface name io-lst)
   #:with ctor-name        (channel-ctor-name #'name)
   #:with (field-name ...) (port-names #'io-lst)
   #:with (param-name ...) (parameter-names #'io-lst)
   #:with (port ...)       (ports #'io-lst)
   #`(begin
       (struct name (field-name ...))
       (define (ctor-name param-name ...)
         (name port ...)))])

; Data port initialization in a channel constructor.
(define-simple-macro (data-port _ ...)
  (box #f))

; Composite port initialization in a channel constructor.
(define-simple-macro (composite-port _ (~optional (multiplicity mult)) _ type arg ...)
   #:with m (or (attribute mult) #'1)
   #:with type-ctor-name (channel-ctor-name #'type)
   (let ([ctor (λ (z) (type-ctor-name arg ...))])
     (if (> m 1)
       (build-vector m ctor)
       (ctor #f))))

; From a component, generate the same output as for an interface,
; and a function with the body of the component.
(define-syntax-parser component
  [(component name io-lst body ...)
   #:with inst-ctor-name   (instance-ctor-name #'name)
   #:with chan-ctor-name   (channel-ctor-name  #'name)
   #:with (param-name ...) (parameter-names #'io-lst)
   #`(begin
       (interface name io-lst)
       (define (inst-ctor-name param-name ...)
         (define chan (chan-ctor-name param-name ...))
         #,@(for/list ([i (in-list (port-names #'io-lst))])
              (define acc (accessor-name #'name i))
              #`(define #,i (#,acc chan)))
         body ...
         chan))])

; An assignment fills the target port's box with the signal
; from the right-hand side.
(define-simple-macro (assignment target expr)
  (set-box! target expr))

; A name expression refers to a variable in the current scope.
(define-simple-macro (name-expr name)
  name)

; After type checking, a field expression contains the name
; of the interface or record type where the field is declared.
; A field expression expands to a field access in a struct instance.
(define-syntax-parser field-expr
  [(field-expr expr name type)
   #:with acc (accessor-name #'type #'name)
   #'(acc expr)])

; An indexed expression expands to a chain of vector accesses.
(define-syntax-parser indexed-expr
  [(indexed-expr expr index ... last)
   #'(vector-ref (indexed-expr index ...) last)]
  [(indexed-expr expr)
   #'expr])

; A signal expression is a wrapper element added by the typechecker
; to identify an expression that refers to a port or local signal
; for reading.
(define-syntax-parser signal-expr
  ; Static case: expr does not depend on other signals.
  [(signal-expr expr)
   #'(signal-proxy (unbox expr))]
  ; Dynamic case: expr contains indices that depend on signals s ...
  [(signal-expr expr (s ...))
   #'((lift (λ (s ...) (signal-first (signal-expr expr)))) s ...)])
