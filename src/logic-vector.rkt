#lang racket

; This module partly reimplements data/bit-vector.
;
; Motivations:
; * bit-vector does not provide access to its internal data representation,
;   which prevents implementing efficient arithmetics.
; * logic-vectors will use a descending indexing scheme when converted
;   from/to lists and strings.
; * At some point, we may support don't care or undefined values.

; TODO change (for ([i (in-range len)])) to (for ([i len]))

(provide
    (contract-out
      [logic-vector?         (-> any/c boolean?)]
      [make-logic-vector     (->* (natural-number/c) (boolean?) logic-vector?)]
      [logic-vector-length   (-> logic-vector? natural-number/c)]
      [logic-vector-ref      (-> logic-vector? natural-number/c boolean?)]
      [list->logic-vector    (-> (listof boolean?) logic-vector?)]
      [logic-vector->list    (-> logic-vector? (listof boolean?))]
      [logic-vector->string  (-> logic-vector? string?)]
      [string->logic-vector  (-> string? logic-vector?)]
      [integer->logic-vector (->* (exact-integer? exact-integer?) (boolean?) logic-vector?)]
      [logic-vector->integer (-> logic-vector? integer?)]))

; This gives the same result as (quotient n 8)
(define (logic-vector-byte-index n)
  (arithmetic-shift n -3))

; This gives the same result as (remainder n 8)
(define (logic-vector-bit-index n)
  (bitwise-and n 7))

(struct logic-vector (bytes length signed?))

(define (make-logic-vector len [signed #f])
  (define-values (bytes-len extra-bits-len) (quotient/remainder len 8))
  (logic-vector (make-bytes (+ bytes-len (if (zero? extra-bits-len) 0 1)) 0)
                len signed))

(define (logic-vector-ref lv n)
  (cond [(< n (logic-vector-length lv))
         (bitwise-bit-set?
             (bytes-ref (logic-vector-bytes lv) (logic-vector-byte-index n))
             (logic-vector-bit-index n))]
        [(logic-vector-signed? lv)
         (logic-vector-msb lv)]
        [else
         #f]))

(define (logic-vector-msb lv)
  (logic-vector-ref lv (sub1 (logic-vector-length lv))))

(define (logic-vector-negative? lv)
  (and (logic-vector-signed? lv) (logic-vector-msb lv)))

; This function is used only internally, so we don't need to check
; (< n (logic-vector-length lv))
(define (logic-vector-set! lv n b)
  (define byte-index (logic-vector-byte-index n))
  (define bit-index  (logic-vector-bit-index  n))
  (define lv-bytes   (logic-vector-bytes      lv))
  (define old-byte   (bytes-ref lv-bytes byte-index))
  (define old-bit    (bitwise-bit-set? old-byte bit-index))
  (unless (eq? b old-bit)
    (define new-byte (bitwise-xor old-byte (arithmetic-shift 1 bit-index)))
    (bytes-set! lv-bytes byte-index new-byte)))

(define (in-reverse-range n)
  (in-range (sub1 n) -1 -1))

(define (list->logic-vector lst)
  (define len (length lst))
  (define lv (make-logic-vector len))
  (for ([i (in-reverse-range len)]
        [b (in-list lst)])
    (logic-vector-set! lv i b))
  lv)

(define (logic-vector->list lv)
  (define len (logic-vector-length lv))
  (for/list ([i (in-reverse-range len)])
    (logic-vector-ref lv i)))

(define (string->logic-vector str)
  (define len (string-length str))
  (define lv (make-logic-vector len))
  (for ([i (in-range len)]
        [j (in-reverse-range len)])
    (when (eqv? (string-ref str i) #\1)
      (logic-vector-set! lv j #t)))
  lv)

(define (logic-vector->string lv)
  (define len (logic-vector-length lv))
  (define str (make-string len))
  (for ([i (in-range len)]
        [j (in-reverse-range len)])
    (string-set! str j (if (logic-vector-ref lv i) #\1 #\0)))
  str)

(define (logic-vector->integer lv)
  (define lv-bytes (logic-vector-bytes lv))
  (define bytes-len (bytes-length lv-bytes))
  ; Pack all bytes into an integer, starting from the most significant.
  (define raw (for/fold ([acc 0])
                        ([i (in-reverse-range bytes-len)])
                ; Shift-left the accumulator and insert the current byte.
                (bitwise-ior (arithmetic-shift acc 8)
                             (bytes-ref lv-bytes i))))
  ; Compute a mask to discard extra bits in the last byte.
  (define mask (sub1 (arithmetic-shift 1 (logic-vector-length lv))))
  ; If lv represents a signed negative, complete the raw value.
  (if (logic-vector-negative? lv)
      (bitwise-not (bitwise-and mask (bitwise-not raw)))
      (bitwise-and mask raw)))

(define (integer->logic-vector val len [signed #f])
  (define lv (make-logic-vector len signed))
  (define lv-bytes (logic-vector-bytes lv))
  (for/fold ([v val])
            ([i (in-range (bytes-length lv-bytes))])
    (bytes-set! lv-bytes i (bitwise-and #xFF v))
    (arithmetic-shift v -8))
  lv)
