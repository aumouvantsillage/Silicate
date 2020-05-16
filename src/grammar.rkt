#lang brag

; We follow this pattern when we want to prevent the parser
; from splicing a list of items in the AST node for a rule:
;
; a: a-item-list
; /a-item-list: a-item*
;
; This will create an AST node of the form: (a (a-item ...))

sil-module: /"module" ID sil-module-item* /"end"

@sil-module-item:
  sil-module |
  sil-interface |
  sil-component

sil-interface: /"interface" ID sil-interface-item-list

/sil-interface-item-list: /"(" sil-interface-item (/"," sil-interface-item)* /","? /")"

@sil-interface-item:
  sil-parameter |
  sil-data-port |
  sil-composite-port |
  sil-inline-composite-port

sil-parameter: ID /":" ("type" | sil-type-expression)

sil-data-port: ID /":" ("in" | "out") sil-type-expression

sil-composite-port: ID sil-multiplicity? /":" ("use" | "flip") sil-name sil-association-list?

sil-inline-composite-port: /"::" ("use" | "flip") sil-name sil-association-list?

@sil-multiplicity: /"[" sil-expression /"]"

; TODO named associations
@sil-association-list: /"(" sil-expression-list? /")"

sil-component: /"component" ID sil-interface-item-list sil-statement-list /"end"

/sil-statement-list: sil-statement*

; TODO other statements
@sil-statement:
  sil-port-assignment

sil-port-assignment:
  sil-indexed-name /"=" sil-expression

sil-name: ID (/"." ID)*

sil-indexed-name: ID (sil-index | /"." ID)*

sil-index: /"[" sil-expression-list /"]"

/sil-expression-list: sil-expression ("," sil-expression)* ","?

; TODO type parameters
@sil-type-expression: sil-name

; TODO other expressions
@sil-expression:
  sil-name |
  INT
