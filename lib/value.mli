open Ppxlib
(** {1 Functions for converting OCaml types to Yaml.value types} *)

val type_to_expr : core_type -> expression

val record_to_expr :
  typ:core_type -> loc:Location.t -> label_declaration list -> expression

val type_decl_to_type : type_declaration -> core_type

val of_yaml_type_to_expr : string option -> core_type -> expression

val of_yaml_record_to_expr :
  loc:Location.t -> label_declaration list -> expression

val monad_fold :
  ('a -> expression) -> expression -> ('a * int) list -> expression

val wrap_open_rresult : loc:location -> expression -> expression
