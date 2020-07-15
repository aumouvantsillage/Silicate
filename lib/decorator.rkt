#lang racket

(require
  "expander.rkt"
  (for-syntax
    syntax/parse
    (prefix-in stx/ "syntax-classes.rkt")
    (prefix-in meta/ "metadata.rkt")
    "scope.rkt"))

(provide (for-syntax decorate))

(begin-for-syntax
  (define (decorate* stx)
    (syntax-parse stx
      [(item ...) (quasisyntax/loc stx #,(map decorate (attribute item)))]
      [_          stx]))

  (define (bind!/meta thunk stx)
    (define meta (thunk stx))
    (define/syntax-parse :stx/named-elt stx)
    (bind! #'name meta)
    (syntax-property stx 'meta meta))

  (define (decorate stx)
    (syntax-parse stx
      #:datum-literals [begin-silicate]
      [(begin-silicate mod) (decorate #'mod)]
      [:stx/module          (with-scope (decorate* stx))]
      [:stx/interface       (bind!/meta meta/make-interface (with-scope (decorate* stx)))]
      [:stx/component       (bind!/meta meta/make-component (with-scope (decorate* stx)))]
      [:stx/parameter       (bind!/meta meta/parameter    (decorate* stx))]
      [:stx/data-port       (bind!/meta meta/data-port    (decorate* stx))]
      [:stx/constant        (bind!/meta meta/constant     (decorate* stx))]
      [:stx/local-signal    (bind!/meta meta/local-signal (decorate* stx))]

      [:stx/composite-port
       #:with mult^ (if (attribute mult) (decorate #'mult) #'(literal-expr 1))
       #:with flip?^ (or (attribute flip?) #'noflip)
       (bind!/meta meta/composite-port
         (quasisyntax/loc stx
           (composite-port name (multiplicity mult^) flip?^ #,(add-scope #'intf-name) #,@(decorate* #'(arg ...)))))]

      [:stx/inline-composite-port
       #:with flip?^ (or (attribute flip?) #'noflip)
       (quasisyntax/loc stx
         (inline-composite-port flip?^ #,(add-scope #'intf-name) #,@(decorate* #'(arg ...))))]

      [:stx/name-expr
       (quasisyntax/loc stx
         (name-expr #,(add-scope #'name)))]

      ; Default case: return the current syntax object.
      [_ (decorate* stx)])))
