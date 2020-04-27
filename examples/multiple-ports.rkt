#lang racket

(require "../src/signal.rkt")
(require "../src/component.rkt")
(require "../src/std.rkt")

; Access to a vector port where the index is a number

(define-interface io ([T type])
  ([out x integer]))

(define-interface mio ([T type] [N positive])
  ([use y (io T) N]))

(define (source N out)
  (for ([k N])
    (interface-set! out mio-y k io-x (static (* 10 (add1 k))))))

(define (regs N in out)
  (for ([k N])
    (interface-set! out mio-y k io-x (register 0 (interface-ref in mio-y k io-x)))))

(define N 3)
(define src-out (make-mio 'intreger N))
(define regs-out (make-mio 'integer N))

(source N src-out)
(regs   N src-out regs-out)

(define L 10)
(for ([k N])
  (printf "~a[~a]: ~a~n" "src-out"  k (signal-take (interface-ref src-out  mio-y k io-x) L))
  (printf "~a[~a]: ~a~n" "regs-out" k (signal-take (interface-ref regs-out mio-y k io-x) L)))

; Access to a vector port where the index is a signal

(define (mux in sel out)
  (define local-sel (interface-ref sel))
  (interface-set! out (interface-ref (local-sel) in mio-y local-sel io-x)))

(define mux-sel (register/r 0 (.= mux-sel (static 2)) (.add1 mux-sel)))
(define mux-out (box #f))
(mux src-out (box mux-sel) mux-out)

(printf "~a: ~a~n" "mux-sel"  (signal-take mux-sel L))
(printf "~a: ~a~n" "mux-out"  (signal-take (interface-ref mux-out) L))
