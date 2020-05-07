
Components
==========

In Silicate, a component is similar to a Verilog module or a VHDL
entity-architecture pair.
Like in VHDL, we can separate the interface declaration (the *entity*) from the
implementation (the *architecture*) but it is not mandatory.

Two key features of Silicate are:

* The ability to define nested interfaces, and to reuse an interface inside another.
* The automatic availability of a *flipped* version of an interface.

Components and interfaces
-------------------------

Components and interfaces will be defined as:

```racket
(component id (parameter ...) (port ...) body ...)

(interface id (parameter ...) (port ...))
```

Ports and parameters
--------------------

Like in VHDL and Verilog, a simple port is defined by:

* its name,
* its mode (`in`, `out`),
* its type,
* optionally, an expression that sets a default input value for `in` ports
  that are not connected.

A composite port is defined by:

* its name,
* its multiplicity (an expression with positive value).
* its mode (`use`, `flip`),
* the target interface name,

When the name and multiplicity of a composite port are omitted, the ports of
its target interface are inlined into the current interface or component.

A parameter is defined by:

* its name,
* its type (a data type, or the keyword `type` for type parameters),
* optionally, an expression that sets a default value.

Implementation
--------------

A component is implemented as a Racket function.
It expects arguments of type:

* `box` for simple ports,
* `struct` for interface ports.

For each interface, a corresponding `struct` type
and a constructor is generated.

A component will read its input ports by `unbox`-ing them.
It will assign its output ports using `set-box!`.
Since components can be instantiated in any order, and can have circular
dependencies, some of its input boxes can be empty when a component is created.
We will use `signal-proxy` for input ports to defer the first `unbox`-ing.

Accessing and assigning ports in an interface hierarchy will have the following forms:

```racket
(signal-proxy (unbox port-designator))

(set-box! port-designator expr)
```

where `port-designator` can be:

```racket
portname
(interfacename-portname port-designator)
(vector-ref port-designator expr)
```

Example:

```racket
(interface intfa ()
    ([p in  integer]
     [q out integer]))

(interface intfb ()
    ([r in  integer]
     [a use intfa 3]
     [s out integer]))

(define-component c () ([t in integer] [b use intfb] [u out integer])
    (define local-t (signal-proxy (unbox t)))
    (define local-r (signal-proxy (unbox (intfb-r b))))
    (define local-p (signal-proxy (unbox (intfa-p (vector-ref (intfb-a b) 2)))))
    ...
    (set-box! u expr-for-u)
    (set-box! (intfb-s b) expr-for-s)
    (set-box! (intfa-q (vector-ref (intfb-a b) 2)) expr-for-q))
```

For convenience, we define macros `port-ref` and `port-set!`
that can be used like this:

```racket
(define local-t (port-ref t))
(define local-r (port-ref b intfb-r))
(define local-p (port-ref b intfb-a 2 intfa-p))
...
(port-set! u expr-for-u)
(port-set! b intfb-s expr-for-s)
(port-set! b intfb-a 2 intfa-q expr-for-q)
```

If we need to accesss a vector port (such as port `a` of `intfb`)
with a signal as the index (e.g. `local-r`),
we can use a variant of `port-ref`:

```racket
(define local-r (port-ref b intfb-r))
(define local-p (port-ref (local-r) b intfb-a local-r intfa-p))
```
