#lang racket

(require
  rackunit
  silicate)

(provide typechecker-tests)

(define typechecker-tests
  (test-suite "Typechecker"
    (test-case "Can label simple signal expressions"
      (begin-silicate
        (module
          (component C
            (data-port a in (name-expr integer))
            (data-port b out (name-expr integer))
            (assignment (name-expr b) (name-expr a)))))

      (define c (make-instance-C))
      (define c-a (static 10))
      (set-box! (C-a c) c-a)

      (define c-b (unbox (C-b c)))
      (check-equal? (signal-take c-b 5) (signal-take c-a 5)))

    (test-case "Can resolve ports in field expressions"
      (begin-silicate
        (module
          (interface I
            (data-port a in  (name-expr integer))
            (data-port b out (name-expr integer)))
          (component J
            (composite-port c I)
            (assignment (field-expr (name-expr c) b)
                        (field-expr (name-expr c) a)))))

      (define j (make-instance-J))
      (define j-c-a (static 10))
      (set-box! (I-a (J-c j)) j-c-a)

      (define j-c-b (unbox (I-b (J-c j))))
      (check-equal? (signal-take j-c-b 5) (signal-take j-c-a 5)))

    (test-case "Can resolve ports in indexed expressions"
      (begin-silicate
        (module
          (interface I
            (data-port a in  (name-expr integer))
            (data-port b out (name-expr integer)))
          (component J
            (composite-port i (multiplicity (literal-expr 2)) I)
            (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) b)
                        (field-expr (indexed-expr (name-expr i) (literal-expr 0)) a))
            (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) b)
                        (field-expr (indexed-expr (name-expr i) (literal-expr 1)) a)))))

      (define j (make-instance-J))
      (define j-i-a-0 (static 10))
      (define j-i-a-1 (static 20))
      (set-box! (I-a (vector-ref (J-i j) 0)) j-i-a-0)
      (set-box! (I-a (vector-ref (J-i j) 1)) j-i-a-1)

      (define j-i-b-0 (unbox (I-b (vector-ref (J-i j) 0))))
      (define j-i-b-1 (unbox (I-b (vector-ref (J-i j) 1))))
      (check-equal? (signal-take j-i-b-0 5) (signal-take j-i-a-0 5))
      (check-equal? (signal-take j-i-b-1 5) (signal-take j-i-a-1 5)))

    (test-case "Can resolve ports in a hierarchy of expressions"
      (begin-silicate
        (module
          (interface I
            (data-port a in  (name-expr integer))
            (data-port b out (name-expr integer)))
          (interface J
            (composite-port i (multiplicity (literal-expr 2)) I))
          (component K
            (composite-port j (multiplicity (literal-expr 2)) J)
            (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 0)) b)
                        (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 0)) a))
            (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 1)) b)
                        (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 1)) a))
            (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 0)) b)
                        (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 0)) a))
            (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 1)) b)
                        (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 1)) a)))))

      (define k (make-instance-K))
      (define k-j-a-0-0 (static 10))
      (define k-j-a-0-1 (static 20))
      (define k-j-a-1-0 (static 30))
      (define k-j-a-1-1 (static 40))
      (set-box! (I-a (vector-ref (J-i (vector-ref (K-j k) 0)) 0)) k-j-a-0-0)
      (set-box! (I-a (vector-ref (J-i (vector-ref (K-j k) 0)) 1)) k-j-a-0-1)
      (set-box! (I-a (vector-ref (J-i (vector-ref (K-j k) 1)) 0)) k-j-a-1-0)
      (set-box! (I-a (vector-ref (J-i (vector-ref (K-j k) 1)) 1)) k-j-a-1-1)

      (define k-j-b-0-0 (unbox (I-b (vector-ref (J-i (vector-ref (K-j k) 0)) 0))))
      (define k-j-b-0-1 (unbox (I-b (vector-ref (J-i (vector-ref (K-j k) 0)) 1))))
      (define k-j-b-1-0 (unbox (I-b (vector-ref (J-i (vector-ref (K-j k) 1)) 0))))
      (define k-j-b-1-1 (unbox (I-b (vector-ref (J-i (vector-ref (K-j k) 1)) 1))))

      (check-equal? (signal-take k-j-b-0-0 5) (signal-take k-j-a-0-0 5))
      (check-equal? (signal-take k-j-b-0-1 5) (signal-take k-j-a-0-1 5))
      (check-equal? (signal-take k-j-b-1-0 5) (signal-take k-j-a-1-0 5))
      (check-equal? (signal-take k-j-b-1-1 5) (signal-take k-j-a-1-1 5)))

    (test-case "Can assign a literal to a signal"
      (begin-silicate
        (module
          (component C
            (data-port a out (name-expr integer))
            (assignment (name-expr a) (literal-expr 10)))))

      (define c (make-instance-C))
      (define c-a (unbox (C-a c)))

      (check-equal? (signal-take c-a 5) (signal-take (static 10) 5)))

    (test-case "Can assign a constant to a signal"
      (begin-silicate
        (module
          (component C
            (data-port a out (name-expr integer))
            (constant c (literal-expr 10))
            (assignment (name-expr a) (name-expr c)))))

      (define c (make-instance-C))
      (define c-a (unbox (C-a c)))

      (check-equal? (signal-take c-a 5) (signal-take (static 10) 5)))

    (test-case "Can assign a static expression to a signal"
      (begin-silicate
        (module
          (component C
            (data-port a out (name-expr integer))
            (constant c (literal-expr 10))
            (assignment (name-expr a)
                        (call-expr + (name-expr c) (literal-expr 1))))))

      (define c (make-instance-C))
      (define c-a (unbox (C-a c)))

      (check-equal? (signal-take c-a 5) (signal-take (static 11) 5)))

    (test-case "Can lift an operation"
      (begin-silicate
        (module
          (component C
            (data-port a in (name-expr integer))
            (data-port b in (name-expr integer))
            (data-port c out (name-expr integer))
            (assignment (name-expr c)
                        (call-expr + (name-expr a) (name-expr b))))))

      (define c (make-instance-C))
      (define c-a (list->signal (list 10 20 30)))
      (define c-b (list->signal (list 40 50 60)))

      (set-box! (C-a c) c-a)
      (set-box! (C-b c) c-b)
      (define c-c (unbox (C-c c)))

      (check-equal? (signal-take c-c 5) (map + (signal-take c-a 5) (signal-take c-b 5))))

    (test-case "Can lift nested calls"
      (begin-silicate
        (module
          (component C
            (data-port a in (name-expr integer))
            (data-port b in (name-expr integer))
            (data-port c in (name-expr integer))
            (data-port d in (name-expr integer))
            (data-port e out (name-expr integer))
            (assignment (name-expr e)
                        (call-expr +
                          (call-expr * (name-expr a) (name-expr b))
                          (call-expr * (name-expr c) (name-expr d)))))))

      (define c (make-instance-C))
      (define c-a (list->signal (list 10 20 30 40 50)))
      (define c-b (static 2))
      (define c-c (list->signal (list 1 2 3 4 5)))
      (define c-d (static 3))

      (set-box! (C-a c) c-a)
      (set-box! (C-b c) c-b)
      (set-box! (C-c c) c-c)
      (set-box! (C-d c) c-d)
      (define c-e (unbox (C-e c)))

      (check-equal? (signal-take c-e 5) (list 23 46 69 92 115)))

    (test-case "Can use local signals"
      (begin-silicate
        (module
          (component C
            (data-port a in (name-expr integer))
            (data-port b in (name-expr integer))
            (data-port c in (name-expr integer))
            (data-port d in (name-expr integer))
            (data-port e out (name-expr integer))
            (local-signal ab (call-expr * (name-expr a) (name-expr b)))
            (local-signal cd (call-expr * (name-expr c) (name-expr d)))
            (assignment (name-expr e)
                        (call-expr + (name-expr ab) (name-expr cd))))))

      (define c (make-instance-C))
      (define c-a (list->signal (list 10 20 30 40 50)))
      (define c-b (static 2))
      (define c-c (list->signal (list 1 2 3 4 5)))
      (define c-d (static 3))

      (set-box! (C-a c) c-a)
      (set-box! (C-b c) c-b)
      (set-box! (C-c c) c-c)
      (set-box! (C-d c) c-d)
      (define c-e (unbox (C-e c)))

      (check-equal? (signal-take c-e 5) (list 23 46 69 92 115)))

    (test-case "Can access simple ports in a vector composite port with dynamic indices"
      (begin-silicate
        (module
          (interface I
            (data-port a in (name-expr integer)))
          (component J
            (composite-port b (multiplicity (literal-expr 3)) I)
            (data-port c in (name-expr integer))
            (data-port d out (name-expr integer))
            (assignment (name-expr d)
                        (field-expr (indexed-expr (name-expr b) (name-expr c)) a)))))

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
      (check-equal? (signal-take j-d (length j-d-lst)) j-d-lst))))

    ; (test-case "Can instantiate a component"
    ;   (begin-silicate
    ;     (module
    ;       (component C
    ;         (parameter N (name-expr integer))
    ;         (data-port a in (name-expr integer))
    ;         (data-port b out (name-expr integer))
    ;         (assignment (name-expr b) (call-expr * (name-expr a) (name-expr N))))
    ;       (component D
    ;         (data-port x in (name-expr integer))
    ;         (data-port y out (name-expr integer))
    ;         (instance c C 10)
    ;         (assignment (field-expr (name-expr c) a) (name-expr x))
    ;         (assignment (name-expr y) (field-expr (name-expr c) b)))))
    ;
    ;   (define d (make-instance-D))
    ;   (define d-x (list->signal (list 10 20 30 40 50)))
    ;   (set-box! (D-x d) d-x)
    ;   (define d-y (unbox (D-y d)))
    ;
    ;   (check-equal? (signal-take d-y 5) (list 100 200 300 400 500)))))

    ; (test-case "Can instantiate a multiple component"
    ;   (begin-silicate
    ;     (module
    ;       (component C
    ;         (parameter N (name-expr integer))
    ;         (data-port a in (name-expr integer))
    ;         (data-port b out (name-expr integer))
    ;         (assignment (name-expr b)
    ;                     (lift-expr [a^ (signal-expr (name-expr a))]
    ;                                (call-expr * a^ N))))
    ;       (component D
    ;         (data-port x in (name-expr integer))
    ;         (data-port y in (name-expr integer))
    ;         (data-port z out (name-expr integer))
    ;         (instance c (multiplicity 2) C 10)
    ;         (assignment (field-expr (indexed-expr (name-expr c) 0) a C) (signal-expr (name-expr x)))
    ;         (assignment (field-expr (indexed-expr (name-expr c) 1) a C) (signal-expr (name-expr y)))
    ;         (assignment (name-expr z) (lift-expr [b0 (signal-expr (field-expr (indexed-expr (name-expr c) 0) b C))]
    ;                                              [b1 (signal-expr (field-expr (indexed-expr (name-expr c) 1) b C))]
    ;                                              (call-expr + b0 b1))))))
    ;   (define d (make-instance-D))
    ;   (define d-x (list->signal (list 10 20 30 40 50)))
    ;   (define d-y (list->signal (list 1 2 3 4 5)))
    ;   (set-box! (D-x d) d-x)
    ;   (set-box! (D-y d) d-y)
    ;   (define d-z (unbox (D-z d)))
    ;
    ;   (check-equal? (signal-take d-z 5) (list 110 220 330 440 550)))))
