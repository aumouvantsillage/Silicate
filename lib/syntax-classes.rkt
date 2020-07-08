#lang racket

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class module
  #:datum-literals [module]
  (pattern (module body ...)))

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
  (pattern (composite-port name (~optional (multiplicity mult)) mode type arg ...)))

(define-syntax-class inline-composite-port
  #:datum-literals [inline-composite-port]
  (pattern (inline-composite-port mode type arg ...)))

(define-syntax-class constant
  #:datum-literals [constant]
  (pattern (constant name type expr)))

(define-syntax-class assignment
  #:datum-literals [assignment]
  (pattern (assignment target expr)))

(define-syntax-class name-expr
  #:datum-literals [name-expr]
  (pattern (name-expr name)))

(define-syntax-class field-expr
  #:datum-literals [field-expr]
  (pattern (field-expr expr name (~optional type))))

(define-syntax-class indexed-expr
  #:datum-literals [indexed-expr]
  (pattern (indexed-expr expr index ...)))

(define-syntax-class literal-expr
  #:datum-literals [literal-expr]
  (pattern (literal-expr value)))
