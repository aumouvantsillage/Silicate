#lang racket

(require
  syntax/parse)

(provide
  create-context
  add-to-context
  module
  interface
  port
  data-port
  composite-port
  inline-composite-port
  parameter
  name)

(define-syntax-class create-context
  (pattern sub:module)
  (pattern sub:interface)
  (pattern sub:component))

(define-syntax-class add-to-context
  (pattern (~or* sub:module
                 sub:interface
                 sub:component
                 sub:parameter
                 sub:data-port
                 sub:composite-port)
    #:attr id (attribute sub.id)))

(define-syntax-class port-mode
  (pattern (~datum in))
  (pattern (~datum out))
  (pattern (~datum use))
  (pattern (~datum flip)))

(define-syntax-class module
  #:datum-literals [module]
  (pattern (module id:identifier item ...)))

(define-syntax-class data-port
  #:datum-literals [data-port]
  (pattern (data-port id:identifier mode:port-mode type)))

(define-syntax-class composite-port
  #:datum-literals [composite-port]
  (pattern (composite-port id:identifier (~optional mult) mode:port-mode type:name arg:expression ...)))

(define-syntax-class inline-composite-port
  #:datum-literals [inline-composite-port]
  (pattern (inline-composite-port mode:port-mode type:name arg:expression ...)))

(define-syntax-class port
  (pattern (~or* sub:data-port sub:composite-port)
    #:attr id (attribute sub.id)))

(define-syntax-class parameter
  #:datum-literals [composite-port]
  (pattern (parameter id:identifier type)))

(define-syntax-class interface
  #:datum-literals [interface]
  (pattern (interface id:identifier (item ...))))

(define-syntax-class component
  #:datum-literals [component]
  (pattern (component id:identifier (item ...) stmt ...)))

(define-syntax-class name
  #:datum-literals [name]
  (pattern (name id:identifier ...)))

(define-syntax-class expression
  (pattern (~or* sub:name sub:number)))
