open Ppxlib

module Backend = struct
  let backend = "yaml"
  let typename = "Yaml.value"
  let suf_to = "to_yaml"
  let suf_of = "of_yaml"

  module Attrs = struct
    let key =
      Attribute.declare "yaml.key" Attribute.Context.label_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)

    let name =
      Attribute.declare "yaml.name" Attribute.Context.constructor_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)

    let default =
      Attribute.declare "yaml.default" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)

    let to_ =
      Attribute.declare "yaml.to_yaml" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)

    let of_ =
      Attribute.declare "yaml.of_yaml" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)
  end

  module Pattern = struct
    open Ast_builder.Default

    let int ~loc i_pat = ppat_variant ~loc "Float" (Some i_pat)
    let float ~loc f = ppat_variant ~loc "Float" (Some f)
    let string ~loc s = ppat_variant ~loc "String" (Some s)
    let bool ~loc b = ppat_variant ~loc "Bool" (Some b)
    let list ~loc l = ppat_variant ~loc "A" (Some l)
    let obj ~loc b = ppat_variant ~loc "O" (Some b)
    let obj_case ~loc ~key ~value = ppat_tuple ~loc [ key; value ]
    let null ~loc = ppat_variant ~loc "Null" None
  end
end

module Value = Ppx_deriving_yaml_common.Make (Backend)

let intf_generator intf = Deriving.Generator.V2.make_noarg intf

let deriver =
  let open Value in
  let of_yaml =
    Deriving.add "of_yaml"
      ~str_type_decl:(impl_generator_of generate_impl_of)
      ~sig_type_decl:(intf_generator generate_intf_of)
  in
  let to_yaml =
    Deriving.add "to_yaml"
      ~str_type_decl:(impl_generator_to generate_impl_to)
      ~sig_type_decl:(intf_generator generate_intf_to)
  in
  Deriving.add_alias "yaml" [ of_yaml; to_yaml ]
