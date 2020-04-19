#lang racket

(require "../src/signal.rkt")
(require "../src/std.rkt")

(struct producer-consumer (data valid ready) #:mutable)

(define (make-producer-consumer)
  (producer-consumer #f #f #f))

(define (producer delay out)
  (define out-ready (signal-proxy (producer-consumer-ready out)))

  (define timer-max (sub1 delay))
  (define timer (register timer-max
                  (if~ out-done
                      (static 0)
                      (if~ (<~ timer (static timer-max))
                        (add1~ timer)
                        timer))))

  (define out-valid (=~ timer (static timer-max)))
  (define out-done (and~ out-valid out-ready))
  (define out-data (register/e 0 out-done (add1~ out-data)))

  (set-producer-consumer-valid! out out-valid)
  (set-producer-consumer-data!  out out-data))

(define (fifo len in out)
  (define in-valid  (signal-proxy (producer-consumer-valid in)))
  (define in-data   (signal-proxy (producer-consumer-data in)))
  (define out-ready (signal-proxy (producer-consumer-ready out)))

  (define count (register/e 0 (xor~ in-done out-done)
                  (if~ in-done
                    (add1~ count)
                    (sub1~ count))))

  (define is-empty (zero?~ count))
  (define is-full  (=~ count (static len)))

  (define index-max (static (sub1 len)))
  (define read-index (register/e 0 out-done
                        (if~ (=~ read-index index-max)
                          (static 0)
                          (add1~ read-index))))
  (define write-index (remainder~ (+~ read-index count) (static len)))

  (define data (register/e (make-vector len 0) in-done
                 (vector-set~ data write-index in-data)))

  (define in-ready (or~ (not~ is-full) out-ready))
  (define in-done  (and~ in-valid in-ready))

  (define out-valid (or~ (not~ is-empty) in-valid))
  (define out-done (and~ out-valid out-ready))
  (define out-data (if~ is-empty in-data (vector-ref~ data read-index)))

  (set-producer-consumer-ready! in  in-ready)
  (set-producer-consumer-valid! out out-valid)
  (set-producer-consumer-data!  out out-data))

(define (consumer delay in)
  (define in-valid (signal-proxy (producer-consumer-valid in)))
  (define in-data  (signal-proxy (producer-consumer-data in)))

  (define timer-max (static (sub1 delay)))
  (define timer (register/re 0 in-done (<~ timer timer-max) (add1~ timer)))

  (define in-ready (=~ timer timer-max))
  (define in-done  (and~ in-valid in-ready))

  (set-producer-consumer-ready! in  in-ready))

(define producer-to-fifo (make-producer-consumer))
(define fifo-to-consumer (make-producer-consumer))

(producer 6 producer-to-fifo)
(fifo 4 producer-to-fifo fifo-to-consumer)
(consumer 3 fifo-to-consumer)

(define (b2i lst)
  (for/list ([b (in-list lst)])
    (if b 1 0)))

(define n 40)
(printf "~a: ~a~n" "in-data  " (signal-take (producer-consumer-data producer-to-fifo)  n))
(printf "~a: ~a~n" "in-valid " (b2i (signal-take (producer-consumer-valid producer-to-fifo) n)))
(printf "~a: ~a~n" "in-ready " (b2i (signal-take (producer-consumer-ready producer-to-fifo) n)))
(printf "~a: ~a~n" "out-data " (signal-take (producer-consumer-data fifo-to-consumer)  n))
(printf "~a: ~a~n" "out-valid" (b2i (signal-take (producer-consumer-valid fifo-to-consumer) n)))
(printf "~a: ~a~n" "out-ready" (b2i (signal-take (producer-consumer-ready fifo-to-consumer) n)))
