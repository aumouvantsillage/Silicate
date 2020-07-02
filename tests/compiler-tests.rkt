#lang racket

(require
  rackunit
  silicate/context-wrapper
  silicate/compiler
  silicate/signal)

(provide compiler-tests)

(define compiler-tests
  (test-suite "Compiler"
    (test-case "Interface with data ports is mapped to channel struct"
      (begin-with-context
        (interface I ([data-port a in  integer]
                      [data-port b out integer])))

      (define an-I (I:channel 10 20))
      (check-eq? (I:channel-a an-I) 10)
      (check-eq? (I:channel-b an-I) 20))

    (test-case "Interface with inline composite ports is mapped to channel struct"
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

    (test-case "Can construct a channel for an interface with simple ports"
      (begin-with-context
        (interface I ([parameter N integer]
                      [data-port a in  integer]
                      [data-port b out integer])))
      (define an-I (make-channel-I 30))
      (check-pred box? (I:channel-a an-I))
      (check-pred box? (I:channel-b an-I)))

    (test-case "Can construct a channel for an interface with composite ports and no parameters"
      (begin-with-context
        (interface I ([data-port a in  integer]
                      [data-port b out integer]))
        (interface J ([data-port c in  integer]
                      [composite-port d use (name I)]
                      [data-port e out integer]))
        (interface K ([data-port f in  integer]
                      [composite-port g use (name J)]
                      [data-port h out integer])))

      (define a-J (make-channel-J))
      (check-pred box? (J:channel-c a-J))
      (check-pred I:channel? (J:channel-d a-J))
      (check-pred box? (I:channel-a (J:channel-d a-J)))
      (check-pred box? (I:channel-b (J:channel-d a-J)))
      (check-pred box? (J:channel-e a-J))

      (define a-K (make-channel-K))
      (check-pred box? (K:channel-f a-K))
      (check-pred J:channel? (K:channel-g a-K))
      (check-pred box? (J:channel-c (K:channel-g a-K)))
      (check-pred box? (I:channel-a (J:channel-d (K:channel-g a-K))))
      (check-pred box? (I:channel-b (J:channel-d (K:channel-g a-K))))
      (check-pred box? (J:channel-e (K:channel-g a-K)))
      (check-pred box? (K:channel-h a-K)))

    (test-case "Can construct a channel for an interface with a vector port"
      (begin-with-context
        (interface I ([data-port a in  integer]
                      [data-port b out integer]))
        (interface J ([composite-port c 3 use (name I)])))

      (define a-J (make-channel-J))
      (check-pred vector? (J:channel-c a-J))
      (check-eq? (vector-length (J:channel-c a-J)) 3)
      (for ([i (range 3)])
        (check-pred I:channel? (vector-ref (J:channel-c a-J) i))))

    (test-case "Can construct a channel for an interface with arguments"
      (begin-with-context
        (interface I ([data-port a in  integer]
                      [data-port b out integer]))
        (interface J ([parameter N integer]
                      [composite-port c (indexed-name N) use (name I)])))
      (define a-J (make-channel-J 3))
      (check-pred vector? (J:channel-c a-J))
      (check-eq? (vector-length (J:channel-c a-J)) 3)
      (for ([i (range 3)])
        (check-pred I:channel? (vector-ref (J:channel-c a-J) i))))

    (test-case "Can construct a channel containing a composite port with arguments"
      (begin-with-context
        (interface I ([data-port a in  integer]
                      [data-port b out integer]))
        (interface J ([parameter N integer]
                      [composite-port c (indexed-name N) use (name I)]))
        (interface K ([parameter M integer]
                      [composite-port d use (name J) (indexed-name M)])))
      (define a-K (make-channel-K 3))
      (check-pred vector? (J:channel-c (K:channel-d a-K)))
      (check-eq? (vector-length (J:channel-c (K:channel-d a-K))) 3)
      (for ([i (range 3)])
        (check-pred I:channel? (vector-ref (J:channel-c (K:channel-d a-K)) i))))

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
      (check-eq? (J:channel-b a-J) 20))

    (test-case "Can construct a channel for a component with simple ports"
      (begin-with-context
        (component C ([parameter N integer]
                      [data-port a in  integer]
                      [data-port b out integer])))
      (define a-C (make-channel-C 30))
      (check-pred box? (C:channel-a a-C))
      (check-pred box? (C:channel-b a-C)))

    (test-case "Can instantiate a component with simple ports"
      (begin-with-context
        (component C ([parameter N integer]
                      [data-port a in  integer]
                      [data-port b out integer])))
      (define a-C (C:component 30))
      (check-pred C:channel? a-C)
      (check-pred box? (C:channel-a a-C))
      (check-pred box? (C:channel-b a-C)))

    (test-case "Can assign a simple port to another simple port"
      (begin-with-context
        (component C ([data-port a in (name integer)]
                      [data-port b out (name integer)])
          (assignment (indexed-name b) (indexed-name a))))
      (define a-C (C:component))
      (define a-C-b (unbox (C:channel-b a-C)))
      (define a-C-a (static 23))
      (set-box! (C:channel-a a-C) a-C-a)
      (check-equal? (signal-take a-C-b 5) (signal-take a-C-a 5)))))
