#lang racket

(require
  rackunit
  silicate/compiler
  (for-syntax
    syntax/parse
    silicate/syntax-classes
    silicate/context))

(provide compiler-tests)

(define-syntax (begin-with-context stx)
  (syntax-parse stx
    [(_ item ...)
     #`(begin #,@(decorate (make-context) #'(item ...)))]))

(define compiler-tests
  (test-suite "Compiler"
    (test-case "Interface with data ports is mapped to struct"
      (interface I ([data-port a in  integer]
                    [data-port b out integer]))
      (define an-I (I:channel 10 20))
      (check-eq? (I:channel-a an-I) 10)
      (check-eq? (I:channel-b an-I) 20))

    (test-case "Interface with inline composite ports is mapped to struct"
      (begin-with-context
        (interface I ([data-port a in  integer]
                      [data-port b out integer]))
        (interface J ([data-port c in  integer]
                      [inline-composite-port use (name I)]
                      [data-port d out integer]))
        (interface K ([data-port e in  integer]
                      [inline-composite-port use (name J)]
                      [data-port f out integer])))
      (define a-J (J:channel 10 20 30 40))
      (check-eq? (J:channel-c a-J) 10)
      (check-eq? (J:channel-a a-J) 20)
      (check-eq? (J:channel-b a-J) 30)
      (check-eq? (J:channel-d a-J) 40)
      (define a-K (K:channel 10 20 30 40 50 60))
      (check-eq? (K:channel-e a-K) 10)
      (check-eq? (K:channel-c a-K) 20)
      (check-eq? (K:channel-a a-K) 30)
      (check-eq? (K:channel-b a-K) 40)
      (check-eq? (K:channel-d a-K) 50)
      (check-eq? (K:channel-f a-K) 60))

    (test-case "Can construct interface with parameters"
      (interface I ([parameter N integer]
                    [data-port a in  integer]
                    [data-port b out integer]))
      (define an-I (make-I:channel 30))
      (check-pred I:channel? an-I))

    (test-case "Can resolve names in a module hierarchy"
      (begin-with-context
        (module M1
          (module M2
            (interface I ([data-port a in  integer]
                          [data-port b out integer]))))
        (module M3
          (module M4
            (interface J ([inline-composite-port use (name M1 M2 I)])))))
      (define a-J (J:channel 10 20))
      (check-eq? (J:channel-a a-J) 10)
      (check-eq? (J:channel-b a-J) 20))))
