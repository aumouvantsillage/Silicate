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
  inline-composite-port |
  constant

@component-item:
  interface-item |
  statement

parameter: ID /":" ("type" | type-expression)

constant: ID /":" /"const" type-expression /"=" expression

data-port: ID /":" ("in" | "out") type-expression

composite-port: ID multiplicity? /":" ("use" | "flip") ID argument-list?

inline-composite-port: /"::" ("use" | "flip") ID argument-list?

multiplicity: /"[" expression /"]"

; TODO named arguments
@argument-list: /"(" (expression /",")* expression? /")"

; TODO other statements
@statement:
  assignment

assignment:
  expression /"=" expression

; TODO other expressions
@expression:
  simple-expr

@simple-expr:
  name-expr |
  field-expr |
  indexed-expr |
  literal-expr |
  /"(" expression /")"

name-expr: ID

field-expr: simple-expr /"." ID

indexed-expr: simple-expr /"[" expression ("," expression)* ","? /"]"

literal-expr: INT

; TODO type parameters
@type-expression: name-expr
