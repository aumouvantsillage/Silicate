#lang brag

; We follow this pattern when we want to prevent the parser
; from splicing a list of items in the AST node for a rule:
;
; a: a-item-list
; /a-item-list: a-item*
;
; This will create an AST node of the form: (a (a-item ...))

module: (interface | component)*

interface: /"interface" ID parameter-list? interface-item* /"end"

component: /"component" ID parameter-list? component-item* /"end"

@parameter-list: /"(" (parameter /",")* parameter? /")"

@interface-item:
  data-port |
  composite-port |
  constant

@component-item:
  interface-item |
  assignment |
  instance

parameter: ID /":" ("type" | type-expression)

constant: /"const" ID /"=" expression

local-signal: /"let" ID /"=" expression

data-port: /"port" ID /":" ("in" | "out") type-expression

composite-port: /"port" ID multiplicity? /":" mode* ID argument-list?

mode: "flip" | "splice"

multiplicity: /"[" expression /"]"

; TODO named arguments
@argument-list: /"(" (expression /",")* expression? /")"

assignment:
  expression /"=" expression

instance:
  /"instance" ID multiplicity? /"=" ID argument-list?

; Expressions ------------------------------------------------------------------

; TODO other expressions
@expression:
  or-expr
  and-expr
  rel-expr
  add-expr
  mult-expr
  prefix-expr
  simple-expr

or-expr:
  (or-expr | and-expr) "or" and-expr

and-expr:
  (and-expr | rel-expr) "and" rel-expr

rel-expr:
  (rel-expr | add-expr) ("<" | ">" | "<=" | ">=" | "==" | "/=") add-expr

add-expr:
  (add-expr | mult-expr) ("+" | "-") mult-expr

mult-expr:
  (mult-expr | prefix-expr) ("*" | "/") prefix-expr

prefix-expr:
  ("-" | "not") simple-expr

@simple-expr:
  literal-expr |
  name-expr |
  field-expr |
  indexed-expr |
  call-expr |
  /"(" expression /")"

literal-expr: INT

name-expr: ID

field-expr:
  simple-expr /"." ID

indexed-expr:
  simple-expr /"[" expression ("," expression)* ","? /"]"

call-expr:
  ID /"(" (expression /",")* expression? /")"

; Type expressions -------------------------------------------------------------

; TODO type parameters
@type-expression: name-expr
