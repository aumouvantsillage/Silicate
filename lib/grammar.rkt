#lang brag

; We follow this pattern when we want to prevent the parser
; from splicing a list of items in the AST node for a rule:
;
; a: a-item-list
; /a-item-list: a-item*
;
; This will create an AST node of the form: (a (a-item ...))

module: (interface | component)*

interface: /"interface" ID interface-item-list

component: /"component" ID interface-item-list statement-list /"end"

/interface-item-list: /"(" interface-item (/"," interface-item)* /","? /")"

@interface-item:
  parameter |
  data-port |
  composite-port |
  inline-composite-port |
  constant

parameter: ID /":" /"param" ("type" | type-expression)

constant: ID /":" /"const" type-expression /"=" expression

data-port: ID /":" ("in" | "out") type-expression

composite-port: ID multiplicity? /":" ("use" | "flip") ID argument-list?

inline-composite-port: /"::" ("use" | "flip") ID argument-list?

multiplicity: /"[" expression /"]"

; TODO named arguments
@argument-list: /"(" expression-list? /")"

/statement-list: statement*

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

indexed-expr: simple-expr /"[" expression-list /"]"

literal-expr: INT

@expression-list: expression ("," expression)* ","?

; TODO type parameters
@type-expression: name
