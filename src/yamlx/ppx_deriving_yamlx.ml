open Ppxlib

module Backend = struct
  let backend = "yamlx"
  let typename = "YAMLx.value"
  let suf_to = "to_yamlx"
  let suf_of = "of_yamlx"

  module Attrs = struct
    let key =
      Attribute.declare "yamlx.key" Attribute.Context.label_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)

    let name =
      Attribute.declare "yamlx.name" Attribute.Context.constructor_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)

    let default =
      Attribute.declare "yamlx.default" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)

    let to_ =
      Attribute.declare "yamlx.to_yamlx" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)

    let of_ =
      Attribute.declare "yamlx.of_yamlx" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)
  end

  module Pattern = struct
    open Ast_builder.Default

    let constr ~loc f =
      let txt = Ldot (Lident "YAMLx", f) in
      { txt; loc }

    let pair_any_something ~loc p = Some (ppat_tuple ~loc [ ppat_any ~loc; p ])

    let int ~loc i_pat =
      ppat_construct ~loc (constr ~loc "Int") (pair_any_something ~loc i_pat)

    let float ~loc f =
      ppat_construct ~loc (constr ~loc "Float") (pair_any_something ~loc f)

    let string ~loc s =
      ppat_construct ~loc (constr ~loc "String") (pair_any_something ~loc s)

    let bool ~loc b =
      ppat_construct ~loc (constr ~loc "Bool") (pair_any_something ~loc b)

    let list ~loc l =
      ppat_construct ~loc (constr ~loc "Seq") (pair_any_something ~loc l)

    let obj ~loc b =
      ppat_construct ~loc (constr ~loc "Map") (pair_any_something ~loc b)

    let obj_case ~loc ~key ~value =
      ppat_tuple ~loc [ ppat_any ~loc; string ~loc key; value ]

    let null ~loc =
      ppat_construct ~loc (constr ~loc "Null") (Some (ppat_any ~loc))
  end
end

module Value = Ppx_deriving_yamlx_common.Make (Backend)

let intf_generator intf = Deriving.Generator.V2.make_noarg intf

let deriver =
  let open Value in
  let of_yaml =
    Deriving.add Backend.suf_of
      ~str_type_decl:(impl_generator_of generate_impl_of)
      ~sig_type_decl:(intf_generator generate_intf_of)
  in
  let to_yaml =
    Deriving.add Backend.suf_to
      ~str_type_decl:(impl_generator_to generate_impl_to)
      ~sig_type_decl:(intf_generator generate_intf_to)
  in
  Deriving.add_alias Backend.backend [ of_yaml; to_yaml ]
