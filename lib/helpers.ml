open Ppxlib

let mkloc txt = { txt; loc = !Ast_helper.default_loc }

let suf_to = "to_yaml"

let suf_of = "of_yaml"

let mangle_name suff ptype_name =
  Ppx_deriving.mangle_lid (`Suffix suff) (lident ptype_name.txt)
