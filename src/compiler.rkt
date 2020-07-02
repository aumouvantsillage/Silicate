#lang racket

(require
  syntax/parse/define
  (for-syntax
    (except-in racket module interface)
    racket/syntax
    syntax/parse
    silicate/syntax-classes
    silicate/context
    silicate/ast))

(provide
  module
  interface
  component
  assignment
  indexed-name
  name)

; FIXME Should we generate Racket modules?
(define-syntax-parser module
  [m:module
   #'(begin m.item ...)])
  ; (module* id #f item ...))

; Convert the given interface id into a struct id
; for channels that instanciate that interface.
(define-for-syntax (channel-struct-id stx)
  (define id (element-id stx))
  (format-id id "~a:channel" id))

; Convert the given interface id into a constructor function id
; for channels that instanciate that interface.
(define-for-syntax (channel-constructor-id stx)
  (define id (element-id stx))
  (format-id id "make-channel-~a" id))

(define-for-syntax (port-accessor-id stx)
  (define id (element-id stx))
  (define sid (channel-struct-id (parent-syntax stx)))
  (format-id id "~a-~a" sid id))

; Convert the given interface id into a constructor function id
; for channels that instanciate that interface.
(define-for-syntax (component-function-id stx)
  (define id (element-id stx))
  (format-id id "~a:component" id))

; Convert an interface into a channel struct type and
; a channel constructor function.
(define-syntax-parser interface
  [i:interface
   #:with sid (channel-struct-id this-syntax)
   #:with cid (channel-constructor-id this-syntax)
   #:with (pt:port ...) (interface-ports this-syntax)
   #:with (pr:parameter ...) (interface-parameters this-syntax)
   #`(begin
       ; Create a struct type with one field for each port of the current interface.
       (struct sid (pt.id ...))
       ; Define a constructor that will recursively construct a channel for each port.
       (define (cid pr.id ...)
         ; Call the default constructor of the above struct type.
         (sid #,@(for/list ([p (syntax->list #'(pt ...))])
                   (syntax-parse p
                     ; Make an empty box for each data port.
                     [d:data-port #'(box #f)]
                     ; Call the appropriate channel constructor for each
                     ; composite port. If the multiplicity of the port is set
                     ; and higher than 1, build a vector.
                     [c:composite-port
                      #:with j:interface (composite-port-interface this-syntax)
                      #:with jcid (channel-constructor-id #'j)
                      #:with mult (or (attribute c.mult) #'1)
                      #'(let ([ctor (λ (z) (jcid c.arg ...))])
                          (if (> mult 1)
                            (build-vector mult ctor)
                            (ctor #f)))])))))])

; Convert a component into a channel struct type,
; a channel constructor function, and an implementation function.
(define-syntax-parser component
  [c:component
   #:with cid (channel-constructor-id this-syntax)
   #:with fid (component-function-id  this-syntax)
   #:with (pr:parameter ...) (interface-parameters this-syntax)
   #`(begin
       ; Generate the same elements as if it were an interface.
       (interface c.id (c.item ...))
       ; Generate a function with the body of this component.
       (define (fid pr.id ...)
         ; Instantiate a channel with the ports of this component.
         (define this-channel (cid pr.id ...))
         ; Define a variable for each port.
         #,@(for/list ([p (interface-ports this-syntax)])
              (syntax-parse p
                [q:port
                 #`(define q.id (#,(port-accessor-id this-syntax) this-channel))]))
         c.body ...
         ; Return the channel with the ports of this component.
         this-channel))])

(define-syntax-parser assignment
  [a:assignment
   #'(set-box! a.value)
   #'(void)])

; Convert a name into a Racket expression.
; FIXME This works only if the name contains only one id
(define-syntax-parser name
  [n:name
   #:with (h ... t) #'(n.id ...)
   #'t])

(define-syntax-parser indexed-name
  [n:indexed-name
   #:with (h ... t) #'(n.item ...)
   (syntax-parse #'t
     [i:index #'(vector-ref (indexed-name (h ...)) t)]
     [id:identifier #'id])])

; (define-syntax-parser indexed-name
;   [n:indexed-name
;    (let ([p (context-resolve this-syntax (list (first (syntax->list #'(p.item ...)))))]))])
