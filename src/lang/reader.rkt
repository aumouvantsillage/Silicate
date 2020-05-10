#lang s-exp syntax/module-reader
silicate/expander
#:read silicate-read
#:read-syntax silicate-read-syntax
#:whole-body-readers? #t

(require silicate/lexer silicate/grammar)

(define (silicate-read in)
  (syntax->datum (silicate-read-syntax #f in)))

(define (silicate-read-syntax src ip)
  (list (parse src (tokenize ip))))
