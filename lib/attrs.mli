open Ppxlib

val key : (label_declaration, label) Attribute.t
val name : (constructor_declaration, string) Attribute.t
val default : (label_declaration, expression) Attribute.t