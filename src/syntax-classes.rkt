#lang racket

(require
  syntax/parse)

(provide
  module
  interface
  port
  data-port
  composite-port
  inline-composite-port
  parameter
  name)

(define-syntax-class module
  #:datum-literals [module]
  (pattern (module id:identifier item ...)))

(define-syntax-class data-port
  #:datum-literals [data-port]
  (pattern (data-port id:identifier mode type)))

(define-syntax-class composite-port
  #:datum-literals [composite-port]
  (pattern (composite-port id:identifier (~optional mult) mode type))
  (pattern (composite-port id:identifier (~optional mult) mode type (arg ...))))

(define-syntax-class inline-composite-port
  #:datum-literals [inline-composite-port]
  (pattern (inline-composite-port mode name arg ...)))

(define-syntax-class port
  (pattern (~or* super:data-port super:composite-port)
    #:attr id (attribute super.id)))

(define-syntax-class parameter
  #:datum-literals [composite-port]
  (pattern (parameter id:identifier type)))

(define-syntax-class interface
  #:datum-literals [interface]
  (pattern (interface id:identifier (item ...))))

(define-syntax-class name
  #:datum-literals [name]
  (pattern (name id:identifier ...)))
