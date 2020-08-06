#lang racket

(require
  rackunit
  silicate
  silicate/lib/std
  "helpers.rkt")

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
      (assignment (name-expr y) (name-expr x)))

    (component C18
      (data-port x in (name-expr integer))
      (data-port y in (name-expr integer))
      (data-port z out (name-expr integer))
      (assignment (name-expr z) (call-expr if (call-expr > (name-expr x) (name-expr y))
                                  (name-expr x)
                                  (name-expr y))))

    (component C19
      (data-port x in (name-expr integer))
      (data-port y out (name-expr integer))
      (assignment (name-expr y) (register-expr 0 x)))))

(define typechecker-tests
  (test-suite "Typechecker"
    (test-case "Can label simple signal expressions"
      (define c (make-instance-C0))
      (define x (static 10))
      (port-set! (c C0-x) x)
      (check-sig-equal? (port-ref c C0-y) x 5))

    (test-case "Can resolve ports in field expressions"
      (define c (make-instance-C1))
      (define x (static 10))
      (port-set! (c C1-i I0-x) x)
      (check-sig-equal? (port-ref c C1-i I0-y) x 5))

    (test-case "Can resolve ports in indexed expressions"
      (define c (make-instance-C2))
      (define x0 (static 10))
      (define x1 (static 20))
      (port-set! (c C2-i 0 I0-x) x0)
      (port-set! (c C2-i 1 I0-x) x1)
      (check-sig-equal? (port-ref c C2-i 0 I0-y) x0 5)
      (check-sig-equal? (port-ref c C2-i 1 I0-y) x1 5))

    (test-case "Can resolve ports in a hierarchy of expressions"
      (define c (make-instance-C3))
      (define x00 (static 10))
      (define x01 (static 20))
      (define x10 (static 30))
      (define x11 (static 40))
      (port-set! (c C3-j 0 I1-i 0 I0-x) x00)
      (port-set! (c C3-j 0 I1-i 1 I0-x) x01)
      (port-set! (c C3-j 1 I1-i 0 I0-x) x10)
      (port-set! (c C3-j 1 I1-i 1 I0-x) x11)
      (check-sig-equal? (port-ref c C3-j 0 I1-i 0 I0-y) x00 5)
      (check-sig-equal? (port-ref c C3-j 0 I1-i 1 I0-y) x01 5)
      (check-sig-equal? (port-ref c C3-j 1 I1-i 0 I0-y) x10 5)
      (check-sig-equal? (port-ref c C3-j 1 I1-i 1 I0-y) x11 5))

    (test-case "Can assign a literal to a signal"
      (define c (make-instance-C4))
      (check-sig-equal? (port-ref c C4-x) (static 10) 5))

    (test-case "Can assign a constant to a signal"
      (define c (make-instance-C5))
      (check-sig-equal? (port-ref c C5-x) (static 10) 5))

    (test-case "Can assign a static expression to a signal"
      (define c (make-instance-C6))
      (check-sig-equal? (port-ref c C6-x) (static 11) 5))

    (test-case "Can lift an operation"
      (define c (make-instance-C7))
      (define x (list->signal (list 10 20 30)))
      (define y (list->signal (list 40 50 60)))
      (port-set! (c C7-x) x)
      (port-set! (c C7-y) y)
      (check-sig-equal? (port-ref c C7-z) (.+ x y) 5))

    (test-case "Can lift nested calls"
      (define c (make-instance-C8))
      (define x (list->signal (list 10 20 30 40 50)))
      (define y (static 2))
      (define z (list->signal (list 1 2 3 4 5)))
      (define u (static 3))
      (port-set! (c C8-x) x)
      (port-set! (c C8-y) y)
      (port-set! (c C8-z) z)
      (port-set! (c C8-u) u)
      (check-sig-equal? (port-ref c C8-v) (.+ (.* x y) (.* z u)) 5))

    (test-case "Can use local signals"
      (define c (make-instance-C9))
      (define x (list->signal (list 10 20 30 40 50)))
      (define y (static 2))
      (define z (list->signal (list 1 2 3 4 5)))
      (define u (static 3))
      (port-set! (c C9-x) x)
      (port-set! (c C9-y) y)
      (port-set! (c C9-z) z)
      (port-set! (c C9-u) u)
      (check-sig-equal? (port-ref c C9-v) (.+ (.* x y) (.* z u)) 5))

    (test-case "Can access simple ports in a vector composite port with dynamic indices"
      (define c (make-instance-C10))
      (define x0 (static 10))
      (define x1 (static 20))
      (define x2 (static 30))
      (define y (list->signal (list 0 1 2 1 0 2)))
      (port-set! (c C10-i 0 I2-x) x0)
      (port-set! (c C10-i 1 I2-x) x1)
      (port-set! (c C10-i 2 I2-x) x2)
      (port-set! (c C10-y)        y)
      (define z (list->signal (list 10 20 30 20 10 30)))
      (check-sig-equal? (port-ref c C10-z) z 5))

    (test-case "Can instantiate a component"
      (define c (make-instance-C12))
      (define x (list->signal (list 10 20 30 40 50)))
      (port-set! (c C12-x) x)
      (check-sig-equal? (port-ref c C12-y) (.* x (static 10)) 5))

    (test-case "Can instantiate a multiple component"
      (define c (make-instance-C13))
      (define x0 (list->signal (list 10 20 30 40 50)))
      (define x1 (list->signal (list 1 2 3 4 5)))
      (port-set! (c C13-x0) x0)
      (port-set! (c C13-x1) x1)
      (check-sig-equal? (port-ref c C13-y) (.* (.+ x0 x1) (static 10)) 5))

    (test-case "Can resolve ports in a spliced interface"
      (define c (make-instance-C14))
      (define x (static 10))
      (port-set! (c C14-x) x)
      (check-sig-equal? (port-ref c C14-y) x 5))

    (test-case "Can resolve ports in a hierarchy from a spliced interface"
      (define c (make-instance-C15))
      (define x0 (static 10))
      (define x1 (static 20))
      (port-set! (c C15-i 0 I0-x) x0)
      (port-set! (c C15-i 1 I0-x) x1)
      (check-sig-equal? (port-ref c C15-i 0 I0-y) x0 5)
      (check-sig-equal? (port-ref c C15-i 1 I0-y) x1 5))

    (test-case "Can resolve ports in an interface with a spliced composite port"
      (define c (make-instance-C16))
      (define x (static 10))
      (port-set! (c C16-j I3-x) x)
      (check-sig-equal? (port-ref c C16-j I3-y) x 5))

    (test-case "Can resolve ports in a doubly spliced composite port"
      (define c (make-instance-C17))
      (define x (static 10))
      (port-set! (c C17-x) x)
      (check-sig-equal? (port-ref c C17-y) x 5))

    (test-case "Can compute a conditional signal"
      (define c (make-instance-C18))
      (define x (list->signal (list 10 20  30 40 50)))
      (define y (list->signal (list 1  200 300 4 5)))
      (port-set! (c C18-x) x)
      (port-set! (c C18-y) y)
      (check-sig-equal? (port-ref c C18-z) ((lift max) x y) 5))

    (test-case "Can register a signal"
      (define c (make-instance-C19))
      (define x (list->signal (list 10 20  30 40 50)))
      (port-set! (c C19-x) x)
      (check-sig-equal? (port-ref c C19-y) (register 0 x) 6))))
