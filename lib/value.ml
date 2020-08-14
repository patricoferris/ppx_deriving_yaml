open Ppxlib
open Ast_helper
open Ast_builder.Default

let rec type_to_expr typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: int] -> [%expr fun x -> `Float (float_of_int x)]
  | [%type: float] -> [%expr fun x -> `Float x]
  | [%type: string] -> [%expr fun x -> `String x]
  | [%type: bool] -> [%expr fun x -> `Bool x]
  | [%type: char] -> [%expr fun x -> `String (String.make 1 x)]
  | [%type: [%t? typ] list] ->
      [%expr fun x -> `A (List.map [%e type_to_expr typ] x)]
  | [%type: [%t? typ] array] ->
      [%expr fun x -> `A Array.(to_list (map [%e type_to_expr typ]) x)]
  | [%type: Yaml.value] -> [%expr fun x -> x]
  | [%type: [%t? typ] option] ->
      [%expr function None -> `Null | Some t -> [%e type_to_expr typ] t]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
      let fwd =
        function_app
          (Exp.ident
             (Helpers.mkloc
                (Ppx_deriving.mangle_lid (`Suffix Helpers.suf_to) lid)))
          args
      in
      [%expr fun x -> [%e fwd] x]
  | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type"

and function_app f l =
  if l = [] then f
  else Exp.apply f (List.map (fun e -> (Nolabel, e)) (List.map type_to_expr l))

let record_to_expr ~loc fields =
  let fields_to_expr fs =
    List.map
      (fun { pld_name; pld_type; pld_loc; _ } ->
        let field =
          Exp.field
            (Ast_builder.Default.evar ~loc "x")
            (Ast_convenience.lid pld_name.txt)
        in
        [%expr
          [%e Ast_builder.Default.estring ~loc:pld_loc pld_name.txt],
            [%e type_to_expr pld_type] [%e field]])
      fs
  in
  let fs = fields_to_expr fields in
  [%expr fun x -> `O [%e Ast_builder.Default.elist ~loc fs]]

let type_decl_to_type ~ptype_name type_decl =
  let loc = type_decl.ptype_loc in
  let t = ptyp_constr ~loc ptype_name [] in
  [%type: [%t t] -> Yaml.value]
