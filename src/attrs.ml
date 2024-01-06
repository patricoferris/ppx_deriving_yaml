open Ppxlib

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

let inline =
  Attribute.declare "yaml.inline" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let to_yaml =
  Attribute.declare "yaml.to_yaml" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)

let of_yaml =
  Attribute.declare "yaml.of_yaml" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
