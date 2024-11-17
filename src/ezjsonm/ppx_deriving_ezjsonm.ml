open Ppxlib

module Backend = struct
  let backend = "ezjsonm"
  let typename = "Ezjsonm.value"
  let suf_to = "to_ezjsonm"
  let suf_of = "of_ezjsonm"

  module Attrs = struct
    let key =
      Attribute.declare "ezjsonm.key" Attribute.Context.label_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)

    let name =
      Attribute.declare "ezjsonm.name" Attribute.Context.constructor_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)

    let default =
      Attribute.declare "ezjsonm.default" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)

    let to_ =
      Attribute.declare "ezjsonm.to_ezjsonm" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)

    let of_ =
      Attribute.declare "ezjsonm.of_ezjsonm" Attribute.Context.label_declaration
        Ast_pattern.(single_expr_payload __)
        (fun x -> x)
  end
end

module Value = Ppx_deriving_ezjsonm_common.Make (Backend)

let intf_generator intf = Deriving.Generator.V2.make_noarg intf

let deriver =
  let open Value in
  let of_ezjsonm =
    Deriving.add "of_ezjsonm"
      ~str_type_decl:(impl_generator_of generate_impl_of)
      ~sig_type_decl:(intf_generator generate_intf_of)
  in
  let to_ezjsonm =
    Deriving.add "to_ezjsonm"
      ~str_type_decl:(impl_generator_to generate_impl_to)
      ~sig_type_decl:(intf_generator generate_intf_to)
  in
  Deriving.add_alias "ezjsonm" [ of_ezjsonm; to_ezjsonm ]
