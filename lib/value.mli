open Ppxlib
(** {1 Functions for converting OCaml types to Yaml.value types} *)

val type_to_expr : core_type -> expression

val record_to_expr : loc:Location.t -> label_declaration list -> expression

val type_decl_to_type :
  ptype_name:longident loc -> type_declaration -> core_type

val of_yaml_type_to_expr : string option -> core_type -> expression

val of_yaml_record_to_expr :
  loc:Location.t -> label_declaration list -> expression
