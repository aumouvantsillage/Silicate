#lang racket

(require "../src/signal.rkt")
(require "../src/component.rkt")
(require "../src/std.rkt")

; Access to a vector port where the index is a number

(interface io ([T type])
  ([x out T]))

(interface mio ([T type] [N positive])
  ([y N use (io T)]))

(component source ([N positive]) ([o use (mio 'integer N)])
  (for ([k N])
    (port-set! o mio-y k io-x (static (* 10 (add1 k))))))

(component regs ([N positive]) ([i flip (mio 'integer N)] [o use (mio 'integer N)])
  (for ([k N])
    (port-set! o mio-y k io-x (register 0 (port-ref i mio-y k io-x)))))

(define N 3)

(define src-out  (make-mio 'integer N))
(define regs-out (make-mio 'integer N))

(source N src-out)
(regs   N src-out regs-out)

(define L 10)
(for ([k N])
  (printf "~a[~a]: ~a~n" "src-out"  k (signal-take (port-ref src-out  mio-y k io-x) L))
  (printf "~a[~a]: ~a~n" "regs-out" k (signal-take (port-ref regs-out mio-y k io-x) L)))

; Access to a vector port where the index is a signal

(define (mux in sel out)
  (define local-sel (port-ref sel))
  (port-set! out (port-ref (local-sel) in mio-y local-sel io-x)))

(define mux-sel (register/r 0 (.= mux-sel (static 2)) (.add1 mux-sel)))
(define mux-out (box #f))
(mux src-out (box mux-sel) mux-out)

(printf "~a: ~a~n" "mux-sel"  (signal-take mux-sel L))
(printf "~a: ~a~n" "mux-out"  (signal-take (port-ref mux-out) L))
