#lang racket

(require
  rackunit
  silicate)

(provide expander-tests)

(define expander-tests
  (test-suite "Expander"
    (test-case "Interface with data ports is mapped to channel struct"
      (interface I ([parameter N #f]
                    [data-port a in  #f]
                    [data-port b out #f]))

      (define i (I 10 20))
      (check-eq? (I-a i) 10)
      (check-eq? (I-b i) 20))

    (test-case "Component with data ports is mapped to channel struct"
      (component I ([parameter N #f]
                    [data-port a in  #f]
                    [data-port b out #f])
        (assignment (name-expr b) (name-expr a)))

      (define i (I 10 20))
      (check-eq? (I-a i) 10)
      (check-eq? (I-b i) 20))

    (test-case "Interface with composite ports is mapped to channel struct"
      (interface I ([parameter N #f]
                    [data-port a in  #f]
                    [data-port b out #f]))
      (interface J ([data-port c in  #f]
                    [composite-port d use I]))

      (define j (J 10 (I 20 30)))
      (check-eq? (J-c j) 10)
      (check-eq? (I-a (J-d j)) 20)
      (check-eq? (I-b (J-d j)) 30))

    (test-case "Component with composite ports is mapped to channel struct"
      (interface I ([parameter N #f]
                    [data-port a in  #f]
                    [data-port b out #f]))
      (component J ([data-port c in  #f]
                    [composite-port d use I])
        (assignment (field-expr (name-expr d) b I) (field-expr (name-expr d) a I)))

      (define j (J 10 (I 20 30)))
      (check-eq? (J-c j) 10)
      (check-eq? (I-a (J-d j)) 20)
      (check-eq? (I-b (J-d j)) 30))

    (test-case "Can construct a channel for an interface with simple ports"
      (interface I ([parameter N #f]
                    [data-port a in  #f]
                    [data-port b out #f]))

      (define i (make-channel-I 30))
      (check-pred box? (I-a i))
      (check-pred box? (I-b i)))

    (test-case "Can construct a channel for a component with simple ports"
      (component I ([parameter N #f]
                    [data-port a in  #f]
                    [data-port b out #f])
        (assignment (name-expr b) (name-expr a)))

      (define i (make-instance-I 30))
      (check-pred box? (I-a i))
      (check-pred box? (I-b i)))

    (test-case "Can construct a channel for an interface with composite ports and no parameters"
      (interface I ([data-port a in  #f]
                    [data-port b out #f]))
      (interface J ([data-port c in  #f]
                    [composite-port d use I]
                    [data-port e out #f]))
      (interface K ([data-port f in  #f]
                    [composite-port g use J]
                    [data-port h out #f]))

      (define j (make-channel-J))
      (check-pred box? (J-c j))
      (check-pred I? (J-d j))
      (check-pred box? (I-a (J-d j)))
      (check-pred box? (I-b (J-d j)))
      (check-pred box? (J-e j))

      (define k (make-channel-K))
      (check-pred box? (K-f k))
      (check-pred J? (K-g k))
      (check-pred box? (J-c (K-g k)))
      (check-pred box? (I-a (J-d (K-g k))))
      (check-pred box? (I-b (J-d (K-g k))))
      (check-pred box? (J-e (K-g k)))
      (check-pred box? (K-h k)))

    (test-case "Can construct a channel for a component with composite ports and no parameters"
      (interface I ([data-port a in  #f]
                    [data-port b out #f]))
      (interface J ([data-port c in  #f]
                    [composite-port d use I]
                    [data-port e out #f]))
      (component K ([data-port f in  #f]
                    [composite-port g use J]
                    [data-port h out #f])
        (assignment (name-expr h) (name-expr f)))

      (define k (make-instance-K))
      (check-pred box? (K-f k))
      (check-pred J? (K-g k))
      (check-pred box? (J-c (K-g k)))
      (check-pred box? (I-a (J-d (K-g k))))
      (check-pred box? (I-b (J-d (K-g k))))
      (check-pred box? (J-e (K-g k)))
      (check-pred box? (K-h k)))))

    ; (test-case "Interface with inline composite ports is mapped to channel struct"
    ;   (begin-with-context
    ;     (interface I ([data-port a in  integer]
    ;                   [data-port b out integer]))
    ;     (interface J ([data-port c in  integer]
    ;                   [inline-composite-port use (name I)]
    ;                   [data-port d out integer]))
    ;     (interface K ([data-port e in  integer]
    ;                   [inline-composite-port use (name J)]
    ;                   [data-port f out integer])))
    ;   (define j (J:channel 10 20 30 40))
    ;   (check-eq? (J:channel-c j) 10)
    ;   (check-eq? (J:channel-a j) 20)
    ;   (check-eq? (J:channel-b j) 30)
    ;   (check-eq? (J:channel-d j) 40)
    ;   (define k (K:channel 10 20 30 40 50 60))
    ;   (check-eq? (K:channel-e k) 10)
    ;   (check-eq? (K:channel-c k) 20)
    ;   (check-eq? (K:channel-a k) 30)
    ;   (check-eq? (K:channel-b k) 40)
    ;   (check-eq? (K:channel-d k) 50)
    ;   (check-eq? (K:channel-f k) 60))
    ;
    ;
    ; (test-case "Can construct a channel for an interface with a vector port"
    ;   (begin-with-context
    ;     (interface I ([data-port a in  integer]
    ;                   [data-port b out integer]))
    ;     (interface J ([composite-port c 3 use (name I)])))
    ;
    ;   (define j (make-channel-J))
    ;   (check-pred vector? (J:channel-c j))
    ;   (check-eq? (vector-length (J:channel-c j)) 3)
    ;   (for ([i (range 3)])
    ;     (check-pred I:channel? (vector-ref (J:channel-c j) i))))
    ;
    ; (test-case "Can construct a channel for an interface with arguments"
    ;   (begin-with-context
    ;     (interface I ([data-port a in  integer]
    ;                   [data-port b out integer]))
    ;     (interface J ([parameter N integer]
    ;                   [composite-port c (indexed-name N) use (name I)])))
    ;   (define j (make-channel-J 3))
    ;   (check-pred vector? (J:channel-c j))
    ;   (check-eq? (vector-length (J:channel-c j)) 3)
    ;   (for ([i (range 3)])
    ;     (check-pred I:channel? (vector-ref (J:channel-c j) i))))
    ;
    ; (test-case "Can construct a channel containing a composite port with arguments"
    ;   (begin-with-context
    ;     (interface I ([data-port a in  integer]
    ;                   [data-port b out integer]))
    ;     (interface J ([parameter N integer]
    ;                   [composite-port c (indexed-name N) use (name I)]))
    ;     (interface K ([parameter M integer]
    ;                   [composite-port d use (name J) (indexed-name M)])))
    ;   (define k (make-channel-K 3))
    ;   (check-pred vector? (J:channel-c (K:channel-d k)))
    ;   (check-eq? (vector-length (J:channel-c (K:channel-d k))) 3)
    ;   (for ([i (range 3)])
    ;     (check-pred I:channel? (vector-ref (J:channel-c (K:channel-d k)) i))))
    ;
    ; (test-case "Can resolve names in a module hierarchy"
    ;   (begin-with-context
    ;     (module M1
    ;       (module M2
    ;         (interface I ([data-port a in  integer]
    ;                       [data-port b out integer]))))
    ;     (module M3
    ;       (module M4
    ;         (interface J ([inline-composite-port use (name M1 M2 I)])))))
    ;   (define j (J:channel 10 20))
    ;   (check-eq? (J:channel-a j) 10)
    ;   (check-eq? (J:channel-b j) 20))
    ;
    ; (test-case "Can construct a channel for a component with simple ports"
    ;   (begin-with-context
    ;     (component C ([parameter N integer]
    ;                   [data-port a in  integer]
    ;                   [data-port b out integer])))
    ;   (define c (make-channel-C 30))
    ;   (check-pred box? (C:channel-a c))
    ;   (check-pred box? (C:channel-b c)))
    ;
    ; (test-case "Can instantiate a component with simple ports"
    ;   (begin-with-context
    ;     (component C ([parameter N integer]
    ;                   [data-port a in  integer]
    ;                   [data-port b out integer])))
    ;   (define c (C:component 30))
    ;   (check-pred C:channel? c)
    ;   (check-pred box? (C:channel-a c))
    ;   (check-pred box? (C:channel-b c)))
    ;
    ; (test-case "Can assign a simple port to another simple port"
    ;   (begin-with-context
    ;     (component C ([data-port a in (name integer)]
    ;                   [data-port b out (name integer)])
    ;       (assignment (indexed-name b) (indexed-name a))))
    ;   (define c (C:component))
    ;   (define c-b (unbox (C:channel-b c)))
    ;   (define c-a (static 23))
    ;   (set-box! (C:channel-a c) c-a)
    ;   (check-equal? (signal-take c-b 5) (signal-take c-a 5)))))
