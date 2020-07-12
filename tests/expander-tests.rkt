#lang racket

(require
  rackunit
  silicate)

(provide expander-tests)

(define expander-tests
  (test-suite "Expander"
    (test-case "Interface with data ports is mapped to channel struct"
      (interface I
        (parameter N #f)
        (data-port a in  #f)
        (data-port b out #f))

      (define i (I 10 20))
      (check-eq? (I-a i) 10)
      (check-eq? (I-b i) 20))

    (test-case "Component with data ports is mapped to channel struct"
      (component I
        (parameter N #f)
        (data-port a in  #f)
        (data-port b out #f)
        (assignment (name-expr b) (signal-expr (name-expr a))))

      (define i (I 10 20))
      (check-eq? (I-a i) 10)
      (check-eq? (I-b i) 20))

    (test-case "Interface with composite ports is mapped to channel struct"
      (interface I
        (parameter N #f)
        (data-port a in  #f)
        (data-port b out #f))
      (interface J
        (data-port c in  #f)
        (composite-port d I))

      (define j (J 10 (I 20 30)))
      (check-eq? (J-c j) 10)
      (check-eq? (I-a (J-d j)) 20)
      (check-eq? (I-b (J-d j)) 30))

    (test-case "Component with composite ports is mapped to channel struct"
      (interface I
        (parameter N #f)
        (data-port a in  #f)
        (data-port b out #f))
      (component J
        (data-port c in  #f)
        (composite-port d I)
        (assignment (field-expr (name-expr d) b I) (signal-expr (field-expr (name-expr d) a I))))

      (define j (J 10 (I 20 30)))
      (check-eq? (J-c j) 10)
      (check-eq? (I-a (J-d j)) 20)
      (check-eq? (I-b (J-d j)) 30))

    (test-case "Can construct a channel for an interface with simple ports"
      (interface I
        (parameter N #f)
        (data-port a in  #f)
        (data-port b out #f))

      (define i (make-channel-I 30))
      (check-pred box? (I-a i))
      (check-pred box? (I-b i)))

    (test-case "Can construct a channel for a component with simple ports"
      (component I
        (parameter N #f)
        (data-port a in  #f)
        (data-port b out #f)
        (assignment (name-expr b) (signal-expr (name-expr a))))

      (define i (make-instance-I 30))
      (check-pred box? (I-a i))
      (check-pred box? (I-b i)))

    (test-case "Can construct a channel for an interface with composite ports and no parameters"
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (interface J
        (data-port c in  #f)
        (composite-port d I)
        (data-port e out #f))
      (interface K
        (data-port f in  #f)
        (composite-port g J)
        (data-port h out #f))

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
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (interface J
        (data-port c in  #f)
        (composite-port d I)
        (data-port e out #f))
      (component K
        (data-port f in  #f)
        (composite-port g J)
        (data-port h out #f)
        (assignment (name-expr h) (signal-expr (name-expr f))))

      (define k (make-instance-K))
      (check-pred box? (K-f k))
      (check-pred J? (K-g k))
      (check-pred box? (J-c (K-g k)))
      (check-pred box? (I-a (J-d (K-g k))))
      (check-pred box? (I-b (J-d (K-g k))))
      (check-pred box? (J-e (K-g k)))
      (check-pred box? (K-h k)))

    (test-case "Can construct a channel for an interface with a vector port"
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (interface J
        (composite-port c (multiplicity 3) I))

      (define j (make-channel-J))
      (check-pred vector? (J-c j))
      (check-eq? (vector-length (J-c j)) 3)
      (for ([i (range 3)])
        (check-pred I? (vector-ref (J-c j) i))))

    (test-case "Can construct a channel for an interface with arguments"
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (interface J
        (parameter N #f)
        (composite-port c (multiplicity (name-expr N)) I))

      (define j (make-channel-J 3))
      (check-pred vector? (J-c j))
      (check-eq? (vector-length (J-c j)) 3)
      (for ([i (range 3)])
        (check-pred I? (vector-ref (J-c j) i))))

    (test-case "Can construct a channel containing a composite port with arguments"
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (interface J
        (parameter N #f)
        (composite-port c (multiplicity (name-expr N)) I))
      (interface K
        (parameter M #f)
        (composite-port d J (name-expr M)))

      (define k (make-channel-K 3))
      (check-pred vector? (J-c (K-d k)))
      (check-eq? (vector-length (J-c (K-d k))) 3)
      (for ([i (range 3)])
        (check-pred I? (vector-ref (J-c (K-d k)) i))))

    (test-case "Can assign a simple port to another simple port"
      (component C
        (data-port a in #f)
        (data-port b out #f)
        (assignment (name-expr b) (signal-expr (name-expr a))))

      (define c (make-instance-C))
      (define c-a (static 23))
      (set-box! (C-a c) c-a)

      (define c-b (unbox (C-b c)))
      (check-equal? (signal-take c-b 5) (signal-take c-a 5)))

    (test-case "Can access simple ports in a composite port"
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (component J
        (composite-port c I)
        (assignment (field-expr (name-expr c) b I)
                    (signal-expr (field-expr (name-expr c) a I))))

      (define j (make-instance-J))
      (define j-c-a (static 23))
      (set-box! (I-a (J-c j)) j-c-a)

      (define j-c-b (unbox (I-b (J-c j))))
      (check-equal? (signal-take j-c-b 5) (signal-take j-c-a 5)))

    (test-case "Can access simple ports in a vector composite port with static indices"
      (interface I
        (data-port a in  #f)
        (data-port b out #f))
      (component J
        (composite-port c (multiplicity 3) I)
        (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 0)) b I)
                    (signal-expr (field-expr (indexed-expr (name-expr c) (literal-expr 0)) a I)))
        (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 1)) b I)
                    (signal-expr (field-expr (indexed-expr (name-expr c) (literal-expr 1)) a I)))
        (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 2)) b I)
                    (signal-expr (field-expr (indexed-expr (name-expr c) (literal-expr 2)) a I))))

      (define j (make-instance-J))
      (define j-c-0-a (static 10))
      (define j-c-1-a (static 20))
      (define j-c-2-a (static 30))
      (set-box! (I-a (vector-ref (J-c j) 0)) j-c-0-a)
      (set-box! (I-a (vector-ref (J-c j) 1)) j-c-1-a)
      (set-box! (I-a (vector-ref (J-c j) 2)) j-c-2-a)

      (define j-c-0-b (unbox (I-b (vector-ref (J-c j) 0))))
      (define j-c-1-b (unbox (I-b (vector-ref (J-c j) 1))))
      (define j-c-2-b (unbox (I-b (vector-ref (J-c j) 2))))
      (check-equal? (signal-take j-c-0-b 5) (signal-take j-c-0-a 5))
      (check-equal? (signal-take j-c-1-b 5) (signal-take j-c-1-a 5))
      (check-equal? (signal-take j-c-2-b 5) (signal-take j-c-2-a 5)))

    (test-case "Can access simple ports in a vector composite port with dynamic indices"
      (interface I
        (data-port a in  #f))
      (component J
        (composite-port b (multiplicity 3) I)
        (data-port c in #f)
        (data-port d out #f)
        (assignment (name-expr d)
                    (lift-expr [c^ (signal-expr (name-expr c))]
                               (signal-expr (field-expr (indexed-expr (name-expr b) c^) a I)))))

      (define j (make-instance-J))
      (define j-b-0-a (static 10))
      (define j-b-1-a (static 20))
      (define j-b-2-a (static 30))
      (set-box! (I-a (vector-ref (J-b j) 0)) j-b-0-a)
      (set-box! (I-a (vector-ref (J-b j) 1)) j-b-1-a)
      (set-box! (I-a (vector-ref (J-b j) 2)) j-b-2-a)

      (define j-c (list->signal (list 0 1 2 1 0 2)))
      (set-box! (J-c j) j-c)

      (define j-d-lst (list 10 20 30 20 10 30))
      (define j-d (unbox (J-d j)))
      (check-equal? (signal-take j-d (length j-d-lst)) j-d-lst))

    (test-case "Can perform an operation between signals"
      (component C
        (data-port a in #f)
        (data-port b in #f)
        (data-port c out #f)
        (assignment (name-expr c)
                    (lift-expr [a^ (signal-expr (name-expr a))]
                               [b^ (signal-expr (name-expr b))]
                               (call-expr + a^ b^))))

      (define la (range 1  5  1))
      (define lb (range 10 50 10))

      (define c (make-instance-C))
      (define c-a (list->signal la))
      (define c-b (list->signal lb))
      (set-box! (C-a c) c-a)
      (set-box! (C-b c) c-b)

      (define c-c (unbox (C-c c)))
      (check-equal? (signal-take c-c (length la)) (map + la lb)))))
