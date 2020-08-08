#lang racket

(require
  rackunit
  silicate
  silicate/lib/std
  "language-fixtures.rkt"
  "helpers.rkt")

(provide language-tests)

(define language-tests
  (test-suite "Language"
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
      (check-sig-equal? (port-ref c C19-y) (register 0 x) 6))

    (test-case "Can register a signal with reset"
      (define c (make-instance-C20))
      (define x (list->signal (list #f #f  #f #t #f)))
      (define y (list->signal (list 10 20  30 40 50)))
      (port-set! (c C20-x) x)
      (port-set! (c C20-y) y)
      (check-sig-equal? (port-ref c C20-z) (register/r 0 x y) 6))

    (test-case "Can register a signal with enable"
      (define c (make-instance-C21))
      (define x (list->signal (list #f #t  #f #t #f)))
      (define y (list->signal (list 10 20  30 40 50)))
      (port-set! (c C21-x) x)
      (port-set! (c C21-y) y)
      (check-sig-equal? (port-ref c C21-z) (register/e 0 x y) 6))

    (test-case "Can register a signal with reset and enable"
      (define c (make-instance-C22))
      (define x (list->signal (list #f #t  #f #t #f)))
      (define y (list->signal (list #f #f  #t #f #f)))
      (define z (list->signal (list 10 20  30 40 50)))
      (port-set! (c C22-x) x)
      (port-set! (c C22-y) y)
      (port-set! (c C22-z) z)
      (check-sig-equal? (port-ref c C22-u) (register/re 0 x y z) 6))))
