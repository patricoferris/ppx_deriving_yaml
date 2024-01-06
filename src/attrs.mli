open Ppxlib

val key : (label_declaration, label) Attribute.t
val name : (constructor_declaration, string) Attribute.t
val default : (label_declaration, expression) Attribute.t
val inline : (label_declaration, unit) Attribute.t
val to_yaml : (label_declaration, expression) Attribute.t
val of_yaml : (label_declaration, expression) Attribute.t
