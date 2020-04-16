#lang racket

; This module partly reimplements data/bit-vector.
; Motivations:
; * bit-vector does not provide access to its internal data representation,
;   which prevents implementing efficient arithmetics.
; * logic-vectors will use a descending indexing scheme when converted
;   from/to lists and strings.
; * At some point, we may support don't care or undefined values.

; TODO contract on upper bound in logic-vector-ref

(provide
    (contract-out
      [logic-vector?       (-> any/c boolean?)]
      [make-logic-vector   (->* (natural-number/c) (boolean?) logic-vector?)]
      [logic-vector-length (-> logic-vector? natural-number/c)]
      [logic-vector-ref    (-> logic-vector? natural-number/c boolean?)]))

(define bits-in-a-word      8)
(define bits-in-a-word-log2 3)

(define largest-word
  (- (expt 2 bits-in-a-word) 1))

; This gives the same result as (quotient n bits-in-a-word)
(define (word-index n)
  (arithmetic-shift n (- bits-in-a-word-log2)))

; This gives the same result as (remainder n bits-in-a-word)
(define (bit-index n)
  (bitwise-and n (sub1 bits-in-a-word)))

(struct logic-vector (words size))

(define (make-logic-vector size [fill #f])
  ; Get the number of bytes to represent the vector internally.
  (define-values (q r) (quotient/remainder size bits-in-a-word))
  ; Add an extra byte if size is not divisible by the size of a word.
  (define size-in-words (+ q (if (zero? r) 0 1)))
  ; Create a byte vector filled as specified by the fill argument.
  (define words (make-bytes size-in-words (if fill largest-word 0)))
  ; If the vector has extra bits and is filled with ones,
  ; set the last byte to 2^r - 1.
  (when (and fill (not (zero? r)))
    (bytes-set! words q (- (expt 2 r) 1)))
  (logic-vector words size))

(define (logic-vector-length lv)
  (logic-vector-size lv))

(define (logic-vector-ref lv n)
  (bitwise-bit-set?
      (bytes-ref (logic-vector-words lv) (word-index n))
      (bit-index n)))
