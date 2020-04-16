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
      [logic-vector?       (-> any/c boolean?)]
      [make-logic-vector   (->* (natural-number/c) (boolean?) logic-vector?)]
      [logic-vector-length (-> logic-vector? natural-number/c)]
      [logic-vector-ref    (-> logic-vector? natural-number/c boolean?)]
      [list->logic-vector  (-> (listof boolean?) logic-vector?)]
      [logic-vector->list  (-> logic-vector? (listof boolean?))]))

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
  (define len-in-words (+ q (if (zero? r) 0 1)))
  ; Create a byte vector filled as specified by the fill argument.
  (define words (make-bytes len-in-words (if fill #xFF 0)))
  ; If the vector has extra bits and is filled with ones,
  ; set the last byte to 2^r - 1.
  (when (and fill (not (zero? r)))
    (bytes-set! words q (- (expt 2 r) 1)))
  (logic-vector words len))

(define (logic-vector-length lv)
  (logic-vector-len lv))

; TODO contract on upper bound
(define (logic-vector-ref lv n)
  (bitwise-bit-set?
      (bytes-ref (logic-vector-words lv) (word-index n))
      (bit-index n)))

; Convert a list to a word.
; The first element of the list is the msb.
(define (list->word l)
  (for/fold ([res 0])
            ([b (in-list l)])
    ; Shift the accumulator left by one bit,
    ; then add the current bit.
    (bitwise-ior (arithmetic-shift res 1)
                 (if b 1 0))))

; Convert a word to a list.
; The first element of the list is the msb.
(define (word->list w)
  (for/list ([i (in-range 7 -1 -1)])
    (bitwise-bit-set? w i)))

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

(define (logic-vector->list lv)
  (define words (logic-vector-words lv))
  (take-right (flatten (reverse (for/list ([i (in-range (bytes-length words))])
                                  (word->list (bytes-ref words i)))))
              (logic-vector-length lv)))
