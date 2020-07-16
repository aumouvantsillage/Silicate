#lang racket

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class named-elt
  #:attributes [name]
  (pattern :interface)
  (pattern :component)
  (pattern :parameter)
  (pattern :data-port)
  (pattern :composite-port)
  (pattern :constant)
  (pattern :local-signal)
  (pattern :instance))

(define-syntax-class module
  #:datum-literals [module]
  (pattern (module body ...)))

(define-syntax-class design-unit
  #:attributes [(param 1) (body 1)]
  (pattern :interface)
  (pattern :component))

(define-syntax-class interface
  #:datum-literals [interface]
  (pattern (interface name param:parameter ... body ...)))

(define-syntax-class component
  #:datum-literals [component]
  (pattern (component name param:parameter ... body ...)))

(define-syntax-class parameter
  #:datum-literals [parameter]
  (pattern (parameter name type)))

(define-syntax-class data-port
  #:datum-literals [data-port]
  (pattern (data-port name mode type)))

(define-syntax-class composite-port
  #:datum-literals [composite-port multiplicity]
  (pattern (composite-port name (~optional (multiplicity mult)) (~optional flip?:flip-mode) intf-name arg ...)))

(define-syntax-class inline-composite-port
  #:datum-literals [inline-composite-port flip use]
  (pattern (inline-composite-port (~optional flip?:flip-mode) intf-name arg ...)))

(define-syntax-class flip-mode
  #:datum-literals [flip noflip]
  (pattern flip)
  (pattern noflip))

(define-syntax-class constant
  #:datum-literals [constant]
  (pattern (constant name expr)))

(define-syntax-class local-signal
  #:datum-literals [local-signal]
  (pattern (local-signal name expr)))

(define-syntax-class assignment
  #:datum-literals [assignment]
  (pattern (assignment target expr)))

(define-syntax-class instance
  #:datum-literals [instance multiplicity]
  (pattern (instance name (~optional (multiplicity mult)) comp-name arg ...)))

(define-syntax-class literal-expr
  #:datum-literals [literal-expr]
  (pattern (literal-expr value)))

(define-syntax-class name-expr
  #:datum-literals [name-expr]
  (pattern (name-expr name)))

(define-syntax-class field-expr
  #:datum-literals [field-expr]
  (pattern (field-expr expr field-name (~optional type-name))))

(define-syntax-class indexed-expr
  #:datum-literals [indexed-expr]
  (pattern (indexed-expr expr index ...)))

(define-syntax-class call-expr
  #:datum-literals [or-expr and-expr rel-expr add-expr mult-expr call-expr prefix-expr]
  (pattern ((~or* or-expr and-expr rel-expr add-expr mult-expr) left fn-name right)
    #:attr (arg 1) #'(left right))
  (pattern (prefix-expr fn-name right)
    #:attr (arg 1) #'(right))
  (pattern (call-expr fn-name arg ...)))

(define-syntax-class lift-expr
  #:datum-literals [lift-expr]
  (pattern (lift-expr binding ...+ expr)))
