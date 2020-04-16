#lang racket

; This module partly reimplements data/bit-vector.
; Motivations:
; * bit-vector does not provide access to its internal data representation,
;   which prevents implementing efficient arithmetics.
; * logic-vectors will use a descending indexing scheme when converted
;   from/to lists and strings.
; * At some point, we may support don't care or undefined values.

(provide
    (contract-out
      [logic-vector?        (-> any/c boolean?)]
      [make-logic-vector    (->* (natural-number/c) (boolean?) logic-vector?)]
      [rename logic-vector-len
       logic-vector-length  (-> logic-vector? natural-number/c)]
      [logic-vector-ref     (-> logic-vector? natural-number/c boolean?)]
      [list->logic-vector   (-> (listof boolean?) logic-vector?)]
      [logic-vector->list   (-> logic-vector? (listof boolean?))]
      [logic-vector->string (-> logic-vector? string?)]
      [string->logic-vector (-> string? logic-vector?)]))

; This gives the same result as (quotient n 8)
(define (word-index n)
  (arithmetic-shift n -3))

; This gives the same result as (remainder n 8)
(define (bit-index n)
  (bitwise-and n 7))

(struct logic-vector (words len))

(define (make-logic-vector len [fill #f])
  ; Get the number of bytes to represent the vector internally.
  (define-values (q r) (quotient/remainder len 8))
  ; Add an extra byte if len is not divisible by the length of a word.
  (define blen (+ q (if (zero? r) 0 1)))
  (logic-vector
    (make-bytes blen (if fill #xFF 0))
    len))

(define (logic-vector-ref lv n)
  (and
    (< n (logic-vector-len lv))
    (bitwise-bit-set?
        (bytes-ref (logic-vector-words lv) (word-index n))
        (bit-index n))))

; Convert a list to a word.
; The first element of the list is the msb.
(define (list->word l)
  (for/fold ([res 0])
            ([b (in-list l)])
    ; Shift the accumulator left by one bit,
    ; then add the current bit.
    (bitwise-ior (arithmetic-shift res 1)
                 (if b 1 0))))

(define (list->logic-vector l)
  ; Create a vector filled with zeros.
  (define res (make-logic-vector (length l)))
  (define words (logic-vector-words res))
  (for/fold ([bv l])
            ([i (in-range (bytes-length words))])
        (cond [(>= (length bv) 8)
               ; Extract the least-significant bits.
               (define-values (left right) (split-at-right bv 8))
               ; Pack them into a byte and store it into the result.
               (bytes-set! words i (list->word right))
               ; Continue with the remaining bits.
               left]
              [else
               (bytes-set! words i (list->word bv))
               empty]))
  res)

(define (string->word s)
  (for/fold ([res 0])
            ([i (in-range (string-length s))])
    (bitwise-ior (arithmetic-shift res 1)
                 (if (eqv? (string-ref s i) #\1) 1 0))))

(define (string->logic-vector s)
  (define len (string-length s))
  (define res (make-logic-vector len))
  (define words (logic-vector-words res))
  (for ([i (in-range (bytes-length words))]
        [j (in-range (- len 8) -8 -8)]
        [k (in-range len 0 -8)])
    (bytes-set! words i (string->word (substring s (max 0 j) k))))
  res)

(define (in-reverse-range n)
  (in-range (sub1 n) -1 -1))

(define (logic-vector->list lv)
  (define len (logic-vector-len lv))
  (for/list ([i (in-reverse-range len)])
    (logic-vector-ref lv i)))

(define (logic-vector->string lv)
  (define len (logic-vector-len lv))
  (define res (make-string len))
  (for ([i (in-range len)]
        [j (in-reverse-range len)])
    (string-set! res j (if (logic-vector-ref lv i) #\1 #\0)))
  res)
