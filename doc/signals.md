
Signals
=======

The model of computation in Silicate is inspired by the
[Cλash language](https://clash-lang.org/).
It allows to model combinatorial, as well as sequential, circuits in a pure
functional style.

Signals in Cλash
----------------

In Cλash, the `Signal` type is defined recursively like this:

```haskell
data Signal a = a :- (Signal a)
```

which means that a signal is a value of type `a`
followed by another signal.
The operator `:-` is used as a constructor.

This is different from a linked list in other languages.
In the definition above, the second operand of `:-` cannot be *null*.
Therefore a signal is always an infinite sequence of values.

This is possible because Cλash is based on Haskell and benefits
from lazy evaluation.
The elements of an infinite sequence are only computed when needed,
for instance when calling the `sampleN` function that returns
the first `n` samples as a list:

```haskell
sampleN 0 _ = []
sampleN n (x :- xs) = x : sampleN (n - 1) xs
```

To define sequential functions with feedback loops, Cλash uses a
technique known as *tying the knot* like this:

```haskell
counter = c where
    c = 0 :- (c + 1)
```

which means that `counter` is a signal `c` where:

* the first sample is 0,
* the rest of the signal is equal to `c + 1`.

This can be confusing at first for people who are not familiar with Haskell.

Signals in Silicate
-------------------

Racket does not have lazy evaluation natively, but we can achieve similar
features with lambda functions and macros.

In Silicate, a signal will be represented by a function that returns a pair
`(val . sig)` where `val` is a sample value and `sig` is another *signal*
(i.e. a function that returns another pair).

The first three functions that we can define to inspect signals are:

* `signal-first`: return the first sample of a signal.
* `signal-rest`: return a signal starting at the second sample of a signal.
* `signal-take`: return a list of the `n` first samples of a signal.

```racket
(define (signal-first x)
  (car (x)))

(define (signal-rest x)
  (cdr (x)))

(define (signal-take x n)
  (if (positive? n)
    (cons (signal-first x) (signal-take (signal-rest x) (sub1 n)))
    empty))
```

Without any syntactic sugar, a signal that returns a constant value can
be written as:

```racket
(define (sig56)
  (cons 56 sig56))

(signal-take sig56 5)
; '(56 56 56 56 56)
```

Now, like in the counter written in Cλash above, we want to be able to create
a signal from a given initial value `x0` and a given expression `expr`.
In Racket, this can be done with a macro:

```racket
(define-syntax-rule (make-signal x0 expr)
  (λ ()
    (cons x0 expr)))
```

This macro is not usable as is.
In fact, in a circuit that contains a feedback loop (e.g. a counter),
the value of the n-th sample depends on the (n-1)-th sample,
which depends on the (n-2)-th and so on.
When computing the n-th sample, we will need to recompute every previous
sample recursively, which will lead to poor performance and stack usage.

A better solution consists in memoizing the pair returned by a signal like this:

```racket
(define-syntax-rule (make-signal x0 expr)
  (let ([res #f])
    (λ ()
      (unless res (set! res (cons x0 expr)))
      res)))
```

To use this macro, we need to provide an expression that evaluates to a signal.
Let's use the `sig56` signal that we have created above:

```racket
(define sig123456 (make-signal 12 (make-signal 34 sig56)))
(signal-take sig123456 5)
; '(12 34 56 56 56)
```

In Cλash, `Functor` and `Applicative` instances are defined on the `Signal`
type, which allows to transform plain functions and operators into
functions that work on signals.
In Silicate, we provide the `lift` function, that accepts a function
with any arity, and a `λ/lift` macro that accepts either a function with
fixed arity, or even a syntactic form like `if`:

```racket
(define (lift f)
  (define (g . x)
    (make-signal
      (apply f (map signal-first x))
      (apply g (map signal-rest  x))))
  g)

(define-syntax λ/lift
  (syntax-rules ()
    ; Create a function with fixed arity.
    [(λ/lift (x ...) body ...)
     (letrec ([f (λ (x ...) body ...)]
              [g (λ (x ...)
                   (make-signal
                     (f (signal-first x) ...)
                     (g (signal-rest x) ...)))])
       g)]
    ; Create a function with variable arity.
    [(λ/lift x body ...)
     (lift (λ x body ...))]))
```

Below, we define versions of `+`, `add1` and `if` for signals:

```racket
(define .+    (lift +))
(define .add1 (λ/lift (x) (add1 x)))
(define .if   (λ/lift (c x y) (if c x y)))

(define sig133557 (.add1 sig123456))
(signal-take sig133557 5)
; '(13 35 57 57 57)

(define sig2569113 (.+ sig123456 sig133557))
(signal-take sig2569113 5)
; '(25 69 113 113 113)
```

Before moving further, we will also add a few macros to create feedback loops:

```racket
; y = y0 . (f y x ...))
(define-syntax-rule (feedback-first y0 (f x ...))
  (letrec ([y (make-signal y0 (f y x ...))]) y))

; y = y0 . (f x ... y))
(define-syntax-rule (feedback-last y0 (f x ...))
  (letrec ([y (make-signal y0 (f x ... y))]) y))

; y = y0 . (f y).
(define-syntax-rule (feedback y0 f)
  (feedback-first y0 (f)))
```

This function creates a signal with a constant value:

```racket
(define (static x0)
  (feedback x0 identity))

(define sig63 (static 63))
(signal-take sig63 5)
; '(63 63 63 63 63)
```

And these macros define various kinds of registers, with or without
*reset* or *enable* inputs:

```racket
; A plain register
(define-syntax-rule (register q0 d)
  (make-signal q0 d))

; Register with synchronous reset.
(define-syntax-rule (register/r q0 r d)
  (register q0 (.if r (static q0) d)))

; Register with enable.
(define-syntax-rule (register/e q0 e d)
  (feedback-last q0 (.if e d)))

; Register with synchronous reset and enable.
(define-syntax-rule (register/re q0 r e d)
  (feedback q0 (λ (q) (.if r (static q0) (.if e d q)))))
```

Now we can define counters like this:

```racket
; A simple counter with no upper limit.
(define counter
  (register 0 (.add1 counter)))

(signal-take counter 5)
; '(0 1 2 3 4)

; A counter mod 4.
(define .= (lift =))
(define counter-mod-4
  (register/r 0 (.= counter-mod-4 (static 3))
                (.add1 counter-mod-4)))

(signal-take counter-mod-4 10)
; '(0 1 2 3 0 1 2 3 0 1)

; A counter that uses counter-mod-4 as a frequency divider.
(define counter-div-4
  (register/e 0 (.= counter-mod-4 (static 3))
                (.add1 counter-div-4)))

(signal-take counter-div-4 13)
; '(0 0 0 0 1 1 1 1 2 2 2 2 3)
```

Finally, like in Cλash, we can promote plain functions into circuit
descriptions following the Medvedev, Moore and Mealy models:

```racket
; f : state input -> state
(define (medvedev s0 f x)
  (feedback-first s0 ((lift f) x)))

; f : state input -> state
; g : state       -> output
(define (moore s0 f g x)
  ((lift g) (medvedev s0 f x)))

; f : state input -> (state output)
(define (mealy s0 f x)
  (letrec ([s  (register s0 ((lift first) so))]
           [so ((lift f) s  x)])
    ((lift second) so)))

; Same as counter-div-4, as a Medvedev machine.
(define counter-div-4-mdv
  (medvedev 0 (λ (c e)
                (if e (add1 c) c))
              (.= counter-mod-4 (static 3))))

(signal-take counter-div-4-mdv 13)
; '(0 0 0 0 1 1 1 1 2 2 2 2 3)

; Same as counter-div-4, with its output multiplied by 10.
(define counter-moore
  (moore 0 (λ (c e)
             (if e (add1 c) c))
           (λ (c)
             (* 10 c))
           (.= counter-mod-4 (static 3))))

(signal-take counter-moore 13)
; '(0 0 0 0 10 10 10 10 20 20 20 20 30)

; Same as counter-div-4, but each value is available in one cycle only.
(define counter-mealy
  (mealy 0 (λ (c e)
             (if e (list (add1 c) c)
                   (list c       -1)))
           (.= counter-mod-4 (static 3))))

(signal-take counter-mealy 13)
# '(-1 -1 -1 0 -1 -1 -1 1 -1 -1 -1 2 -1)
```
