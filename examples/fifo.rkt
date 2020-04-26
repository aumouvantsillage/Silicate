#lang racket

(require "../src/signal.rkt")
(require "../src/component.rkt")
(require "../src/std.rkt")

(define-interface producer
  (out data  T)
  (out valid boolean)
  (in  ready boolean))

(define (source delay out)
  (define out-ready (interface-ref out producer-ready))

  (define timer-max (sub1 delay))
  (define timer (register timer-max
                  (.if out-done
                      (static 0)
                      (.if (.< timer (static timer-max))
                        (.add1 timer)
                        timer))))

  (define out-valid (.= timer (static timer-max)))
  (define out-done (.and out-valid out-ready))
  (define out-data (register/e 0 out-done (.add1 out-data)))

  (interface-set! out producer-valid out-valid)
  (interface-set! out producer-data  out-data))

(define (fifo len in out)
  (define in-valid  (interface-ref in  producer-valid))
  (define in-data   (interface-ref in  producer-data))
  (define out-ready (interface-ref out producer-ready))

  (define count (register/e 0 (.xor in-done out-done)
                  (.if in-done
                    (.add1 count)
                    (.sub1 count))))

  (define is-empty (.zero? count))
  (define is-full  (.= count (static len)))

  (define index-max (static (sub1 len)))
  (define read-index (register/e 0 out-done
                        (if (.= read-index index-max)
                          (static 0)
                          (.add1 read-index))))
  (define write-index (.remainder (.+ read-index count) (static len)))

  (define data (register/e (make-vector len 0) in-done
                 (.vector-set data write-index in-data)))

  (define in-ready (.or (.not is-full) out-ready))
  (define in-done  (.and in-valid in-ready))

  (define out-valid (.or (.not is-empty) in-valid))
  (define out-done (.and out-valid out-ready))
  (define out-data (.if is-empty in-data (.vector-ref data read-index)))

  (interface-set! in  producer-ready in-ready)
  (interface-set! out producer-valid out-valid)
  (interface-set! out producer-data  out-data))

(define (sink delay in)
  (define in-valid (interface-ref in producer-valid))
  (define in-data  (interface-ref in producer-data))

  (define timer-max (static (sub1 delay)))
  (define timer (register/re 0 in-done (.< timer timer-max) (.add1 timer)))

  (define in-ready (.= timer timer-max))
  (define in-done  (.and in-valid in-ready))

  (interface-set! in producer-ready in-ready))

(define fifo-in (make-producer))
(define fifo-out (make-producer))

(source 6 fifo-in)
(fifo   4 fifo-in fifo-out)
(sink   3 fifo-out)

(define (b2i lst)
  (for/list ([b (in-list lst)])
    (if b 1 0)))

(define n 40)
(printf "~a: ~a~n" "in-data  "      (signal-take (interface-ref fifo-in  producer-data)  n))
(printf "~a: ~a~n" "in-valid " (b2i (signal-take (interface-ref fifo-in  producer-valid) n)))
(printf "~a: ~a~n" "in-ready " (b2i (signal-take (interface-ref fifo-in  producer-ready) n)))
(printf "~a: ~a~n" "out-data "      (signal-take (interface-ref fifo-out producer-data)  n))
(printf "~a: ~a~n" "out-valid" (b2i (signal-take (interface-ref fifo-out producer-valid) n)))
(printf "~a: ~a~n" "out-ready" (b2i (signal-take (interface-ref fifo-out producer-ready) n)))
