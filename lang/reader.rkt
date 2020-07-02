#lang s-exp syntax/module-reader
silicate/lang/expander
#:read silicate-read
#:read-syntax silicate-read-syntax
#:whole-body-readers? #t

(require silicate/lib/lexer silicate/lib/grammar)

(define (silicate-read in)
  (syntax->datum (silicate-read-syntax #f in)))

(define (silicate-read-syntax src ip)
  (list (parse src (tokenize ip))))
