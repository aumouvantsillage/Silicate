#lang racket

(require
  "expander.rkt"
  (for-syntax
    racket
    racket/dict
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

  (define (bind!/meta thunk stx . args)
    (define meta (apply thunk stx args))
    (define/syntax-parse :stx/named-elt stx)
    (bind! #'name meta)
    (syntax-property stx 'meta meta))

  (define (flip m)
    (match m
      [(meta/data-port stx mode)
       (meta/data-port stx (if (eq? 'in mode) 'out 'in))]
      [(meta/composite-port stx flip?)
       (meta/composite-port stx (not flip?))]
      [_ m]))

  (define (decorate-spliced lst)
    (apply append
      (for/list ([i lst])
        (syntax-parse (decorate i)
          [:stx/composite-port #:when (attribute splice?)
           ; If the current item is a spliced composite port.
           ; Get the dict containing the declarations of the target interface.
           (define sc (meta/design-unit-local-scope (lookup #'intf-name meta/interface?)))
           ; Create a list with the current item, followed by aliases
           ; for the fields of the target interface.
           (cons this-syntax
             (for/list ([(field-name field-meta) (in-dict sc)])
               ; If the current composite port is flipped, flip the
               ; field's mode in the metadata.
               (define field-meta^ (if (attribute flip?) (flip field-meta) field-meta))
               ; Bring the target field into the current scope.
               (bind! field-name field-meta^)
               ; Create an alias syntax object that will be used in the code generator.
               (syntax-property
                 (quasisyntax/loc this-syntax
                   (alias #,field-name name intf-name))
                 'meta field-meta^)))]

          [_ (list this-syntax)]))))

  (define (decorate stx)
    (syntax-parse stx
      #:datum-literals [begin-silicate]
      [(begin-silicate mod) (decorate #'mod)]
      [:stx/module          (with-scope (decorate* stx))]
      [:stx/parameter       (bind!/meta meta/parameter    (decorate* stx))]
      [:stx/data-port       (bind!/meta meta/data-port    (decorate* stx) (syntax-e #'mode))]
      [:stx/constant        (bind!/meta meta/constant     (decorate* stx))]
      [:stx/local-signal    (bind!/meta meta/local-signal (decorate* stx))]

      [:stx/interface
       (bind!/meta meta/make-interface
         (with-scope
           (quasisyntax/loc stx
             (interface name #,@(decorate* #'(param ...))  #,@(decorate-spliced (attribute body))))))]

      [:stx/component
       (bind!/meta meta/make-component
         (with-scope
           (quasisyntax/loc stx
             (component name #,@(decorate* #'(param ...))  #,@(decorate-spliced (attribute body))))))]

      [:stx/composite-port
       #:with mult^ (if (attribute mult) (decorate #'mult) #'(literal-expr 1))
       (bind!/meta meta/composite-port
         (quasisyntax/loc stx
           (composite-port name (multiplicity mult^) mode ... #,(add-scope #'intf-name) #,@(decorate* #'(arg ...))))
         (attribute flip?))]

      [:stx/instance
       #:with mult^ (if (attribute mult) (decorate #'mult) #'(literal-expr 1))
       (bind!/meta meta/instance
         (quasisyntax/loc stx
           (instance name (multiplicity mult^) #,(add-scope #'comp-name) #,@(decorate* #'(arg ...)))))]

      [:stx/name-expr
       (quasisyntax/loc stx
         (name-expr #,(add-scope #'name)))]

      ; Default case: return the current syntax object.
      [_ (decorate* stx)])))
