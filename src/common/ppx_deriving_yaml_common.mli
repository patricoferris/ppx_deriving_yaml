open Ppxlib
(** {1 Functions for converting OCaml types to Yaml.value types} *)

module type Backend = sig
  val backend : string
  val typename : string
  val suf_to : string
  val suf_of : string

  module Attrs : sig
    val key : (label_declaration, label) Attribute.t
    val name : (constructor_declaration, string) Attribute.t
    val default : (label_declaration, expression) Attribute.t
    val to_ : (label_declaration, expression) Attribute.t
    val of_ : (label_declaration, expression) Attribute.t
  end
end

module Make (_ : Backend) : sig
  val type_to_expr : core_type -> expression

  val record_to_expr :
    typ:core_type -> loc:Location.t -> label_declaration list -> expression

  val type_decl_to_type : type_declaration -> core_type
  val type_decl_of_type : type_declaration -> core_type
  val of_backend_type_to_expr : string option -> core_type -> expression

  val of_backend_record_to_expr :
    loc:Location.t -> skip_unknown:bool -> label_declaration list -> expression

  val monad_fold :
    ('a -> expression) -> expression -> ('a * int) list -> expression

  val wrap_open_rresult : loc:location -> expression -> expression

  val generate_impl_to :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    structure_item list

  val generate_impl_of :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    bool ->
    structure_item list

  val generate_intf_to :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    signature_item list

  val generate_intf_of :
    ctxt:Expansion_context.Deriver.t ->
    rec_flag * type_declaration list ->
    signature_item list

  val impl_generator_to :
    (ctxt:Expansion_context.Deriver.t -> 'a -> 'b) ->
    ('b, 'a) Deriving.Generator.t

  val impl_generator_of :
    (ctxt:Expansion_context.Deriver.t -> 'a -> bool -> 'b) ->
    ('b, 'a) Deriving.Generator.t
end
