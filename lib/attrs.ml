open Ppxlib
let key =
  Attribute.declare
    "yaml.key"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)

    let name =
      Attribute.declare
        "yaml.name"
        Attribute.Context.constructor_declaration
        Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
        (fun x -> x)