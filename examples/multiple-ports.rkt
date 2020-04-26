#lang racket

(require "../src/signal.rkt")
(require "../src/component.rkt")
(require "../src/std.rkt")

; Access to a vector port where the index is a number

(define-interface io
  (out x T))

(define-interface mio
  (use y io 3))

(define (source out)
  (for ([k 3])
    (interface-set! out mio-y k io-x (static (* 10 (add1 k))))))

(define (regs in out)
  (for ([k 3])
    (interface-set! out mio-y k io-x (register 0 (interface-ref in mio-y k io-x)))))

(define src-out (make-mio))
(define regs-out (make-mio))

(source src-out)
(regs src-out regs-out)

(define n 10)
(for ([k 3])
  (printf "~a[~a]: ~a~n" "src-out"  k (signal-take (interface-ref src-out  mio-y k io-x) n))
  (printf "~a[~a]: ~a~n" "regs-out" k (signal-take (interface-ref regs-out mio-y k io-x) n)))

; Access to a vector port where the index is a signal

(define (mux in sel out)
  (define local-sel (interface-ref sel))
  (interface-set! out (interface-ref (local-sel) in mio-y local-sel io-x))

(define mux-sel (register/r 0 (.= mux-sel (static 2)) (.add1 mux-sel))))
(define mux-out (box #f))
(mux src-out (box mux-sel) mux-out)

(printf "~a: ~a~n" "mux-sel"  (signal-take mux-sel n))
(printf "~a: ~a~n" "mux-out"  (signal-take (interface-ref mux-out) n))
