#lang racket

(require
  rackunit
  silicate/compiler
  (for-syntax silicate/context))

(provide compiler-tests)

(define-syntax (begin-with-context stx)
  (syntax-case stx ()
    [(_ item ...)
     #`(begin #,@(decorate (make-context) #'(item ...)))]))

(define compiler-tests
  (test-suite "Compiler"
    (test-case "Interface with data ports is mapped to struct"
      (sil-interface I ([sil-data-port a in  integer]
                        [sil-data-port b out integer]))
      (define an-I (I:channel 10 20))
      (check-eq? (I:channel-a an-I) 10)
      (check-eq? (I:channel-b an-I) 20))

    (test-case "Interface with inline composite ports is mapped to struct"
      (begin-with-context
        (sil-interface I ([sil-data-port a in  integer]
                          [sil-data-port b out integer]))
        (sil-interface J ([sil-data-port c in  integer]
                          [sil-inline-composite-port use (sil-name I)]
                          [sil-data-port d out integer]))
        (sil-interface K ([sil-data-port e in  integer]
                          [sil-inline-composite-port use (sil-name J)]
                          [sil-data-port f out integer])))
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
      (check-eq? (K:channel-f a-K) 60))))
