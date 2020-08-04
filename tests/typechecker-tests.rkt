#lang racket

(require
  rackunit
  silicate)

(provide typechecker-tests)

(begin-silicate
  (module
    (component C0
      (data-port x in (name-expr integer))
      (data-port y out (name-expr integer))
      (assignment (name-expr y) (name-expr x)))

    (interface I0
      (data-port x in (name-expr integer))
      (data-port y out (name-expr integer)))

    (component C1
      (composite-port i I0)
      (assignment (field-expr (name-expr i) y) (field-expr (name-expr i) x)))

    (component C2
      (composite-port i (multiplicity (literal-expr 2)) I0)
      (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y)
                  (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x))
      (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y)
                  (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x)))

    (interface I1
      (composite-port i (multiplicity (literal-expr 2)) I0))

    (component C3
      (composite-port j (multiplicity (literal-expr 2)) I1)
      (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 0)) y)
                  (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 0)) x))
      (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 1)) y)
                  (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 0)) i) (literal-expr 1)) x))
      (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 0)) y)
                  (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 0)) x))
      (assignment (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 1)) y)
                  (field-expr (indexed-expr (field-expr (indexed-expr (name-expr j) (literal-expr 1)) i) (literal-expr 1)) x)))

    (component C4
      (data-port x out (name-expr integer))
      (assignment (name-expr x) (literal-expr 10)))

    (component C5
      (data-port x out (name-expr integer))
      (constant k (literal-expr 10))
      (assignment (name-expr x) (name-expr k)))

    (component C6
      (data-port x out (name-expr integer))
      (constant k (literal-expr 10))
      (assignment (name-expr x) (call-expr + (name-expr k) (literal-expr 1))))

    (component C7
      (data-port x in (name-expr integer))
      (data-port y in (name-expr integer))
      (data-port z out (name-expr integer))
      (assignment (name-expr z) (call-expr + (name-expr x) (name-expr y))))

    (component C8
      (data-port x in (name-expr integer))
      (data-port y in (name-expr integer))
      (data-port z in (name-expr integer))
      (data-port u in (name-expr integer))
      (data-port v out (name-expr integer))
      (assignment (name-expr v) (call-expr + (call-expr * (name-expr x) (name-expr y))
                                             (call-expr * (name-expr z) (name-expr u)))))

    (component C9
      (data-port x in (name-expr integer))
      (data-port y in (name-expr integer))
      (data-port z in (name-expr integer))
      (data-port u in (name-expr integer))
      (data-port v out (name-expr integer))
      (local-signal xy (call-expr * (name-expr x) (name-expr y)))
      (local-signal zu (call-expr * (name-expr z) (name-expr u)))
      (assignment (name-expr v) (call-expr + (name-expr xy) (name-expr zu))))

    (interface I2
      (data-port x in (name-expr integer)))

    (component C10
      (composite-port i (multiplicity (literal-expr 3)) I2)
      (data-port y in (name-expr integer))
      (data-port z out (name-expr integer))
      (assignment (name-expr z) (field-expr (indexed-expr (name-expr i) (name-expr y)) x)))

    (component C11
      (parameter N (name-expr integer))
      (data-port x in (name-expr integer))
      (data-port y out (name-expr integer))
      (assignment (name-expr y) (call-expr * (name-expr x) (name-expr N))))

    (component C12
      (data-port x in (name-expr integer))
      (data-port y out (name-expr integer))
      (instance c C11 (literal-expr 10))
      (assignment (field-expr (name-expr c) x) (name-expr x))
      (assignment (name-expr y) (field-expr (name-expr c) y)))

    (component C13
      (data-port x0 in (name-expr integer))
      (data-port x1 in (name-expr integer))
      (data-port y out (name-expr integer))
      (instance c (multiplicity (literal-expr 2)) C11 (literal-expr 10))
      (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 0)) x) (name-expr x0))
      (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 1)) x) (name-expr x1))
      (assignment (name-expr y) (call-expr + (field-expr (indexed-expr (name-expr c) (literal-expr 0)) y)
                                             (field-expr (indexed-expr (name-expr c) (literal-expr 1)) y))))

    (component C14
      (composite-port i splice I0)
      (assignment (name-expr y) (name-expr x)))

    (component C15
      (composite-port j splice I1)
      (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y)
                  (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x))
      (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y)
                  (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x)))

    (interface I3
      (composite-port i splice I0))

    (component C16
      (composite-port j I3)
      (assignment (field-expr (name-expr j) y) (field-expr (name-expr j) x)))

    (component C17
      (composite-port j splice I3)
      (assignment (name-expr y) (name-expr x)))))

(define (check-port-equal? t e n)
  (define e^ (if (box? e) (unbox e) e))
  (check-equal? (signal-take (unbox t) n) (signal-take e^ n)))

(define typechecker-tests
  (test-suite "Typechecker"
    (test-case "Can label simple signal expressions"
      (define c (make-instance-C0))
      (set-box! (C0-x c) (static 10))
      (check-port-equal? (C0-y c) (C0-x c) 5))

    (test-case "Can resolve ports in field expressions"
      (define c (make-instance-C1))
      (define i (C1-i c))
      (set-box! (I0-x i) (static 10))
      (check-port-equal? (I0-y i) (I0-x i) 5))

    (test-case "Can resolve ports in indexed expressions"
      (define c (make-instance-C2))
      (define c-i-0-x (static 10))
      (define c-i-1-x (static 20))
      (set-box! (I0-x (vector-ref (C2-i c) 0)) c-i-0-x)
      (set-box! (I0-x (vector-ref (C2-i c) 1)) c-i-1-x)
      (define c-i-0-y (unbox (I0-y (vector-ref (C2-i c) 0))))
      (define c-i-1-y (unbox (I0-y (vector-ref (C2-i c) 1))))
      (check-equal? (signal-take c-i-0-y 5) (signal-take c-i-0-x 5))
      (check-equal? (signal-take c-i-1-y 5) (signal-take c-i-1-x 5)))

    (test-case "Can resolve ports in a hierarchy of expressions"
      (define c (make-instance-C3))
      (define c-j-0-i-0-x (static 10))
      (define c-j-0-i-1-x (static 20))
      (define c-j-1-i-0-x (static 30))
      (define c-j-1-i-1-x (static 40))
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 0)) 0)) c-j-0-i-0-x)
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 0)) 1)) c-j-0-i-1-x)
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 1)) 0)) c-j-1-i-0-x)
      (set-box! (I0-x (vector-ref (I1-i (vector-ref (C3-j c) 1)) 1)) c-j-1-i-1-x)
      (define c-j-0-i-0-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 0)) 0))))
      (define c-j-0-i-1-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 0)) 1))))
      (define c-j-1-i-0-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 1)) 0))))
      (define c-j-1-i-1-y (unbox (I0-y (vector-ref (I1-i (vector-ref (C3-j c) 1)) 1))))
      (check-equal? (signal-take c-j-0-i-0-y 5) (signal-take c-j-0-i-0-x 5))
      (check-equal? (signal-take c-j-0-i-1-y 5) (signal-take c-j-0-i-1-x 5))
      (check-equal? (signal-take c-j-1-i-0-y 5) (signal-take c-j-1-i-0-x 5))
      (check-equal? (signal-take c-j-1-i-1-y 5) (signal-take c-j-1-i-1-x 5)))

    (test-case "Can assign a literal to a signal"
      (define c (make-instance-C4))
      (check-port-equal? (C4-x c) (static 10) 5))

    (test-case "Can assign a constant to a signal"
      (define c (make-instance-C5))
      (check-port-equal? (C5-x c) (static 10) 5))

    (test-case "Can assign a static expression to a signal"
      (define c (make-instance-C6))
      (check-port-equal? (C6-x c) (static 11) 5))

    (test-case "Can lift an operation"
      (define c (make-instance-C7))
      (define c-x (list->signal (list 10 20 30)))
      (define c-y (list->signal (list 40 50 60)))
      (set-box! (C7-x c) c-x)
      (set-box! (C7-y c) c-y)
      (define c-z (unbox (C7-z c)))
      (check-equal? (signal-take c-z 5) (map + (signal-take c-x 5) (signal-take c-y 5))))

    (test-case "Can lift nested calls"
      (define c (make-instance-C8))
      (define c-x (list->signal (list 10 20 30 40 50)))
      (define c-y (static 2))
      (define c-z (list->signal (list 1 2 3 4 5)))
      (define c-u (static 3))
      (set-box! (C8-x c) c-x)
      (set-box! (C8-y c) c-y)
      (set-box! (C8-z c) c-z)
      (set-box! (C8-u c) c-u)
      (define c-v (unbox (C8-v c)))
      (check-equal? (signal-take c-v 5) (list 23 46 69 92 115)))

    (test-case "Can use local signals"
      (define c (make-instance-C9))
      (define c-x (list->signal (list 10 20 30 40 50)))
      (define c-y (static 2))
      (define c-z (list->signal (list 1 2 3 4 5)))
      (define c-u (static 3))
      (set-box! (C9-x c) c-x)
      (set-box! (C9-y c) c-y)
      (set-box! (C9-z c) c-z)
      (set-box! (C9-u c) c-u)
      (define c-v (unbox (C9-v c)))
      (check-equal? (signal-take c-v 5) (list 23 46 69 92 115)))

    (test-case "Can access simple ports in a vector composite port with dynamic indices"
      (define c (make-instance-C10))
      (define c-i-0-x (static 10))
      (define c-i-1-x (static 20))
      (define c-i-2-x (static 30))
      (set-box! (I2-x (vector-ref (C10-i c) 0)) c-i-0-x)
      (set-box! (I2-x (vector-ref (C10-i c) 1)) c-i-1-x)
      (set-box! (I2-x (vector-ref (C10-i c) 2)) c-i-2-x)
      (define c-y (list->signal (list 0 1 2 1 0 2)))
      (set-box! (C10-y c) c-y)
      (define c-z-lst (list 10 20 30 20 10 30))
      (define c-z (unbox (C10-z c)))
      (check-equal? (signal-take c-z (length c-z-lst)) c-z-lst))

    (test-case "Can instantiate a component"
      (define c (make-instance-C12))
      (define c-x (list->signal (list 10 20 30 40 50)))
      (set-box! (C12-x c) c-x)
      (define c-y (unbox (C12-y c)))
      (check-equal? (signal-take c-y 5) (list 100 200 300 400 500)))

    (test-case "Can instantiate a multiple component"
      (define c (make-instance-C13))
      (define c-x0 (list->signal (list 10 20 30 40 50)))
      (define c-x1 (list->signal (list 1 2 3 4 5)))
      (set-box! (C13-x0 c) c-x0)
      (set-box! (C13-x1 c) c-x1)
      (define c-y (unbox (C13-y c)))
      (check-equal? (signal-take c-y 5) (list 110 220 330 440 550)))

    (test-case "Can resolve ports in a spliced interface"
      (define c (make-instance-C14))
      (set-box! (C14-x c) (static 10))
      (check-port-equal? (C14-y c) (C14-x c) 5))

    (test-case "Can resolve ports in a hierarchy from a spliced interface"
      (define c (make-instance-C15))
      (define c-i-0-x (static 10))
      (define c-i-1-x (static 20))
      (set-box! (I0-x (vector-ref (C15-i c) 0)) c-i-0-x)
      (set-box! (I0-x (vector-ref (C15-i c) 1)) c-i-1-x)
      (define c-i-0-y (unbox (I0-y (vector-ref (C15-i c) 0))))
      (define c-i-1-y (unbox (I0-y (vector-ref (C15-i c) 1))))
      (check-equal? (signal-take c-i-0-y 5) (signal-take c-i-0-x 5))
      (check-equal? (signal-take c-i-1-y 5) (signal-take c-i-1-x 5)))

    (test-case "Can resolve ports in an interface with a spliced composite port"
      (define c (make-instance-C16))
      (define j (C16-j c))
      (set-box! (I3-x j) (static 10))
      (check-port-equal? (I3-y j) (I3-x j) 5))

    (test-case "Can resolve ports in a doubly spliced composite port"
      (define c (make-instance-C17))
      (set-box! (C17-x c) (static 10))
      (check-port-equal? (C17-y c) (C17-x c) 5))))
