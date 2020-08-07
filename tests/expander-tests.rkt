#lang racket

(require
  rackunit
  silicate
  "helpers.rkt")

(provide expander-tests)

(interface I0
  (parameter N #f)
  (data-port x in  #f)
  (data-port y out #f))

(component C0
  (parameter N #f)
  (data-port x in  #f)
  (data-port y out #f)
  (assignment (name-expr y) (signal-expr (name-expr x))))

(interface I1
  (composite-port i I0)
  (data-port z in #f))

(component C1
  (composite-port i I0)
  (data-port z in #f)
  (assignment (field-expr (name-expr i) y I0) (signal-expr (field-expr (name-expr i) x I0))))

(interface I2
  (data-port x in #f)
  (data-port y out #f))

(interface I3
  (data-port z in #f)
  (composite-port i I2)  (data-port u out #f))

(interface I4
  (data-port v in #f)
  (composite-port j I3)
  (data-port w out #f))

(component C2
  (data-port v in #f)
  (composite-port j I3)
  (data-port w out #f)
  (assignment (name-expr w) (signal-expr (name-expr v))))

(interface I5
  (composite-port i (multiplicity 3) I2))

(interface I6
  (parameter N #f)
  (composite-port i (multiplicity (name-expr N)) I2))

(interface I7
  (parameter M #f)
  (composite-port j I6 (name-expr M)))

(component C3
  (composite-port i I2)
  (assignment (field-expr (name-expr i) y I2)
              (signal-expr (field-expr (name-expr i) x I2))))

(component C4
  (composite-port i (multiplicity 3) I2)
  (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y I2)
              (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x I2)))
  (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y I2)
              (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x I2)))
  (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 2)) y I2)
              (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 2)) x I2))))

(interface I8
  (data-port x in (name-expr integer)))

(component C5
  (composite-port i (multiplicity (literal-expr 3)) I8)
  (data-port y in (name-expr integer))
  (data-port z out (name-expr integer))
  (assignment (name-expr z)
              (lift-expr [y^ (signal-expr (name-expr y))]
                         (signal-expr (field-expr (indexed-expr (name-expr i) (name-expr y^)) x I8)))))

(component C6
  (data-port x in #f)
  (data-port y in #f)
  (data-port z out #f)
  (assignment (name-expr z)
              (lift-expr [x^ (signal-expr (name-expr x))]
                         [y^ (signal-expr (name-expr y))]
                         (call-expr + (name-expr x^) (name-expr y^)))))

(component C7
  (data-port x in #f)
  (data-port y in #f)
  (data-port z in #f)
  (data-port u in #f)
  (data-port v out #f)
  (local-signal xy (lift-expr [x^ (signal-expr (name-expr x))]
                              [y^ (signal-expr (name-expr y))]
                              (call-expr * (name-expr x^) (name-expr y^))))
  (local-signal zu (lift-expr [z^ (signal-expr (name-expr z))]
                              [u^ (signal-expr (name-expr u))]
                              (call-expr * (name-expr z^) (name-expr u^))))
  (assignment (name-expr v)
              (lift-expr [xy^ (signal-expr (name-expr xy))]
                         [zu^ (signal-expr (name-expr zu))]
                         (call-expr + (name-expr xy^) (name-expr zu^)))))

(component C8
  (parameter N #f)
  (data-port x in #f)
  (data-port y out #f)
  (assignment (name-expr y)
              (lift-expr [x^ (signal-expr (name-expr x))]
                         (call-expr * x^ N))))
(component C9
  (data-port x in #f)
  (data-port y out #f)
  (instance c C8 10)
  (assignment (field-expr (name-expr c) x C8) (signal-expr (name-expr x)))
  (assignment (name-expr y) (signal-expr (field-expr (name-expr c) y C8))))

(component C10
  (data-port x0 in #f)
  (data-port x1 in #f)
  (data-port y out #f)
  (instance c (multiplicity 2) C8 10)
  (assignment (field-expr (indexed-expr (name-expr c) 0) x C8) (signal-expr (name-expr x0)))
  (assignment (field-expr (indexed-expr (name-expr c) 1) x C8) (signal-expr (name-expr x1)))
  (assignment (name-expr y) (lift-expr [y0 (signal-expr (field-expr (indexed-expr (name-expr c) 0) y C8))]
                                       [y1 (signal-expr (field-expr (indexed-expr (name-expr c) 1) y C8))]
                                       (call-expr + y0 y1))))

(component C11
  (data-port x in #f)
  (data-port y out #f)
  (assignment (name-expr y) (register-expr (literal-expr 0) (signal-expr (name-expr x)))))

(component C12
  (data-port x in #f)
  (data-port y in #f)
  (data-port z out #f)
  (assignment (name-expr z) (register-expr (literal-expr 0) (when-clause (signal-expr (name-expr x)))
                                           (signal-expr (name-expr y)))))

(component C13
  (data-port x in #f)
  (data-port y in #f)
  (data-port z out #f)
  (assignment (name-expr z) (register-expr (literal-expr 0)
                                           (signal-expr (name-expr y)) (when-clause (signal-expr (name-expr x))))))

(component C14
  (data-port x in #f)
  (data-port y in #f)
  (data-port z in #f)
  (data-port u out #f)
  (assignment (name-expr u) (register-expr (literal-expr 0) (when-clause (signal-expr (name-expr x)))
                                           (signal-expr (name-expr z)) (when-clause (signal-expr (name-expr y))))))

(define expander-tests
  (test-suite "Expander"
    (test-case "Interface with data ports is mapped to channel struct"
      (define i (I0 10 20))
      (check-eq? (I0-x i) 10)
      (check-eq? (I0-y i) 20))

    (test-case "Component with data ports is mapped to channel struct"
      (define c (C0 10 20))
      (check-eq? (C0-x c) 10)
      (check-eq? (C0-y c) 20))

    (test-case "Interface with composite ports is mapped to channel struct"
      (define j (I1 (I0 10 20) 30))
      (check-eq? (I0-x (I1-i j)) 10)
      (check-eq? (I0-y (I1-i j)) 20)
      (check-eq? (I1-z j) 30))

    (test-case "Component with composite ports is mapped to channel struct"
      (define c (C1 (I0 10 20) 30))
      (check-eq? (I0-x (C1-i c)) 10)
      (check-eq? (I0-y (C1-i c)) 20)
      (check-eq? (C1-z c) 30))

    (test-case "Can construct a channel for an interface with simple ports"
      (define i (make-channel-I0 30))
      (check-pred box? (I0-x i))
      (check-pred box? (I0-y i)))

    (test-case "Can construct a channel for a component with simple ports"
      (define c (make-instance-C0 30))
      (check-pred box? (C0-x c))
      (check-pred box? (C0-y c)))

    (test-case "Can construct a channel for an interface with composite ports and no parameters"
      (define i3 (make-channel-I3))
      (check-pred box? (I3-z i3))
      (check-pred I2? (I3-i i3))
      (check-pred box? (I2-x (I3-i i3)))
      (check-pred box? (I2-y (I3-i i3)))
      (check-pred box? (I3-u i3))

      (define i4 (make-channel-I4))
      (check-pred box? (I4-v i4))
      (check-pred I3? (I4-j i4))
      (check-pred box? (I3-z (I4-j i4)))
      (check-pred box? (I2-x (I3-i (I4-j i4))))
      (check-pred box? (I2-y (I3-i (I4-j i4))))
      (check-pred box? (I3-u (I4-j i4)))
      (check-pred box? (I4-w i4)))

    (test-case "Can construct a channel for a component with composite ports and no parameters"
      (define c (make-instance-C2))
      (check-pred box? (C2-v c))
      (check-pred I3? (C2-j c))
      (check-pred box? (I3-z (C2-j c)))
      (check-pred box? (I2-x (I3-i (C2-j c))))
      (check-pred box? (I2-y (I3-i (C2-j c))))
      (check-pred box? (I3-u (C2-j c)))
      (check-pred box? (C2-w c)))

    (test-case "Can construct a channel for an interface with a vector port"
      (define j (make-channel-I5))
      (check-pred vector? (I5-i j))
      (check-eq? (vector-length (I5-i j)) 3)
      (for ([n (range 3)])
        (check-pred I2? (vector-ref (I5-i j) n))))

    (test-case "Can construct a channel for an interface with arguments"
      (define j (make-channel-I6 5))
      (check-pred vector? (I6-i j))
      (check-eq? (vector-length (I6-i j)) 5)
      (for ([n (range 5)])
        (check-pred I2? (vector-ref (I6-i j) n))))

    (test-case "Can construct a channel containing a composite port with arguments"
      (define k (make-channel-I7 3))
      (check-pred vector? (I6-i (I7-j k)))
      (check-eq? (vector-length (I6-i (I7-j k))) 3)
      (for ([n (range 3)])
        (check-pred I2? (vector-ref (I6-i (I7-j k)) n))))

    (test-case "Can assign a simple port to another simple port"
      (define c (make-instance-C0 #f))
      (define x (static 23))
      (port-set! (c C0-x) x)
      (check-sig-equal? (port-ref c C0-y) x 5))

    (test-case "Can access simple ports in a composite port"
      (define c (make-instance-C3))
      (define x (static 23))
      (port-set! (c C3-i I2-x) x)
      (check-sig-equal? (port-ref c C3-i I2-y) x 5))

    (test-case "Can access simple ports in a vector composite port with static indices"
      (define c (make-instance-C4))
      (define x0 (static 10))
      (define x1 (static 20))
      (define x2 (static 30))
      (port-set! (c C4-i 0 I2-x) x0)
      (port-set! (c C4-i 1 I2-x) x1)
      (port-set! (c C4-i 2 I2-x) x2)
      (check-sig-equal? (port-ref c C4-i 0 I2-x) x0 5)
      (check-sig-equal? (port-ref c C4-i 1 I2-x) x1 5)
      (check-sig-equal? (port-ref c C4-i 2 I2-x) x2 5))

    (test-case "Can access simple ports in a vector composite port with dynamic indices"
      (define c (make-instance-C5))
      (define x0 (static 10))
      (define x1 (static 20))
      (define x2 (static 30))
      (define y (list->signal (list 0 1 2 1 0 2)))
      (port-set! (c C5-i 0 I8-x) x0)
      (port-set! (c C5-i 1 I8-x) x1)
      (port-set! (c C5-i 2 I8-x) x2)
      (port-set! (c C5-y)        y)
      (define z (list->signal (list 10 20 30 20 10 30)))
      (check-sig-equal? (port-ref c C5-z) z 5))

    (test-case "Can perform an operation between signals"
      (define c (make-instance-C6))
      (define x (list->signal (range 1  5  1)))
      (define y (list->signal (range 10 50 10)))
      (port-set! (c C6-x) x)
      (port-set! (c C6-y) y)
      (check-sig-equal? (port-ref c C6-z) (.+ x y) 5))

    (test-case "Can use local signals"
      (define c (make-instance-C7))
      (define x (list->signal (list 10 20 30 40 50)))
      (define y (static 2))
      (define z (list->signal (list 1 2 3 4 5)))
      (define u (static 3))
      (port-set! (c C7-x) x)
      (port-set! (c C7-y) y)
      (port-set! (c C7-z) z)
      (port-set! (c C7-u) u)
      (check-sig-equal? (port-ref c C7-v) (.+ (.* x y) (.* z u)) 5))

    (test-case "Can instantiate a component"
      (define c (make-instance-C9))
      (define x (list->signal (list 10 20 30 40 50)))
      (port-set! (c C9-x) x)
      (check-sig-equal? (port-ref c C9-y) (.* x (static 10)) 5))

    (test-case "Can instantiate a multiple component"
      (define c (make-instance-C10))
      (define x0 (list->signal (list 10 20 30 40 50)))
      (define x1 (list->signal (list 1 2 3 4 5)))
      (port-set! (c C10-x0) x0)
      (port-set! (c C10-x1) x1)
      (check-sig-equal? (port-ref c C10-y) (.* (.+ x0 x1) (static 10)) 5))

    (test-case "Can register a signal"
      (define c (make-instance-C11))
      (define x (list->signal (list 10 20  30 40 50)))
      (port-set! (c C11-x) x)
      (check-sig-equal? (port-ref c C11-y) (register 0 x) 6))

    (test-case "Can register a signal with reset"
      (define c (make-instance-C12))
      (define x (list->signal (list #f #f  #f #t #f)))
      (define y (list->signal (list 10 20  30 40 50)))
      (port-set! (c C12-x) x)
      (port-set! (c C12-y) y)
      (check-sig-equal? (port-ref c C12-z) (register/r 0 x y) 6))

    (test-case "Can register a signal with enable"
      (define c (make-instance-C13))
      (define x (list->signal (list #f #t  #f #t #f)))
      (define y (list->signal (list 10 20  30 40 50)))
      (port-set! (c C13-x) x)
      (port-set! (c C13-y) y)
      (check-sig-equal? (port-ref c C13-z) (register/e 0 x y) 6))

    (test-case "Can register a signal with reset and enable"
      (define c (make-instance-C14))
      (define x (list->signal (list #f #t  #f #t #f)))
      (define y (list->signal (list #f #f  #t #f #f)))
      (define z (list->signal (list 10 20  30 40 50)))
      (port-set! (c C14-x) x)
      (port-set! (c C14-y) y)
      (port-set! (c C14-z) z)
      (check-sig-equal? (port-ref c C14-u) (register/re 0 x y z) 6))))
