#lang brag

; We follow this pattern when we want to prevent the parser
; from splicing a list of items in the AST node for a rule:
;
; a: a-item-list
; /a-item-list: a-item*
;
; This will create an AST node of the form: (a (a-item ...))

module: /"module" ID module-item-list /"end"

/module-item-list: module-item*

@module-item:
  module |
  interface |
  component

interface: /"interface" ID interface-item-list

/interface-item-list: /"(" interface-item (/"," interface-item)* /","? /")"

@interface-item:
  parameter |
  data-port |
  composite-port |
  inline-composite-port

parameter: ID /":" ("type" | type-expression)

data-port: ID /":" ("in" | "out") type-expression

composite-port: ID multiplicity? /":" ("use" | "flip") name association-list?

inline-composite-port: /"::" ("use" | "flip") name association-list?

@multiplicity: /"[" expression /"]"

; TODO named associations
@association-list: /"(" expression-list? /")"

component: /"component" ID interface-item-list statement-list /"end"

/statement-list: statement*

; TODO other statements
@statement:
  port-assignment

port-assignment:
  indexed-name /"=" expression

name: ID (/"." ID)*

indexed-name: ID (index | /"." ID)*

index: /"[" expression-list /"]"

/expression-list: expression ("," expression)* ","?

; TODO type parameters
@type-expression: name

; TODO other expressions
@expression:
  name |
  INT
