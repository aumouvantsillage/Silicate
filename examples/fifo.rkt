#lang racket

(require "../src/signal.rkt")
(require "../src/component.rkt")
(require "../src/std.rkt")

(interface producer ([T type])
  ([data  out T]
   [valid out boolean]
   [ready in  boolean]))

(component source ([delay positive]) ([p use (producer 'natural)])
  (define p-ready (port-ref p producer-ready))

  (define timer-max (sub1 delay))
  (define timer (register timer-max
                  (.if p-done
                      (static 0)
                      (.if (.< timer (static timer-max))
                        (.add1 timer)
                        timer))))

  (define p-valid (.= timer (static timer-max)))
  (define p-done (.and p-valid p-ready))
  (define p-data (register/e 0 p-done (.add1 p-data)))

  (port-set! p producer-valid p-valid)
  (port-set! p producer-data  p-data))

(component fifo ([len positive]) ([c flip (producer 'natural)] [p use (producer 'natural)])
  (define c-valid (port-ref c producer-valid))
  (define c-data  (port-ref c producer-data))
  (define p-ready (port-ref p producer-ready))

  (define count (register/e 0 (.xor c-done p-done)
                  (.if c-done
                    (.add1 count)
                    (.sub1 count))))

  (define is-empty (.zero? count))
  (define is-full  (.= count (static len)))

  (define index-max (static (sub1 len)))
  (define read-index (register/e 0 p-done
                        (if (.= read-index index-max)
                          (static 0)
                          (.add1 read-index))))
  (define write-index (.remainder (.+ read-index count) (static len)))

  (define data (register/e (make-vector len 0) c-done
                 (.vector-set data write-index c-data)))

  (define c-ready (.or (.not is-full) p-ready))
  (define c-done  (.and c-valid c-ready))

  (define p-valid (.or (.not is-empty) c-valid))
  (define p-done (.and p-valid p-ready))
  (define p-data (.if is-empty c-data (.vector-ref data read-index)))

  (port-set! c producer-ready c-ready)
  (port-set! p producer-valid p-valid)
  (port-set! p producer-data  p-data))

(component sink ([delay positive]) ([c flip (producer 'natural)])
  (define c-valid (port-ref c producer-valid))
  (define c-data  (port-ref c producer-data))

  (define timer-max (static (sub1 delay)))
  (define timer (register/re 0 c-done (.< timer timer-max) (.add1 timer)))

  (define c-ready (.= timer timer-max))
  (define c-done  (.and c-valid c-ready))

  (port-set! c producer-ready c-ready))

(define fifo-c (make-producer 'integer))
(define fifo-p (make-producer 'integer))

(source 6 fifo-c)
(fifo   4 fifo-c fifo-p)
(sink   3 fifo-p)

(define (b2i lst)
  (for/list ([b (in-list lst)])
    (if b 1 0)))

(define n 40)
(printf "~a: ~a~n" "c-data  "      (signal-take (port-ref fifo-c producer-data)  n))
(printf "~a: ~a~n" "c-valid " (b2i (signal-take (port-ref fifo-c producer-valid) n)))
(printf "~a: ~a~n" "c-ready " (b2i (signal-take (port-ref fifo-c producer-ready) n)))
(printf "~a: ~a~n" "p-data  "      (signal-take (port-ref fifo-p producer-data)  n))
(printf "~a: ~a~n" "p-valid " (b2i (signal-take (port-ref fifo-p producer-valid) n)))
(printf "~a: ~a~n" "p-ready " (b2i (signal-take (port-ref fifo-p producer-ready) n)))
