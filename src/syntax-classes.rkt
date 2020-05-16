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
  #:datum-literals [sil-module]
  (pattern (sil-module id:identifier item ...)))

(define-syntax-class data-port
  #:datum-literals [sil-data-port]
  (pattern (sil-data-port id:identifier mode type)))

(define-syntax-class composite-port
  #:datum-literals [sil-composite-port]
  (pattern (sil-composite-port id:identifier (~optional mult) mode type))
  (pattern (sil-composite-port id:identifier (~optional mult) mode type (arg ...))))

(define-syntax-class inline-composite-port
  #:datum-literals [sil-inline-composite-port]
  (pattern (sil-inline-composite-port mode name arg ...)))

(define-syntax-class port
  (pattern (~or* super:data-port super:composite-port)
    #:attr id (attribute super.id)))

(define-syntax-class parameter
  #:datum-literals [sil-composite-port]
  (pattern (sil-parameter id:identifier type)))

(define-syntax-class interface
  #:datum-literals [sil-interface]
  (pattern (sil-interface id:identifier (item ...))))

(define-syntax-class name
  #:datum-literals [sil-name]
  (pattern (sil-name id:identifier ...)))
