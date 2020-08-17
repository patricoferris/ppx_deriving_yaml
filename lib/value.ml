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
  | { ptyp_desc = Ptyp_var name; _ } ->
      let ident = Exp.ident (Ast_convenience.lid ("poly_" ^ name)) in
      [%expr ([%e ident] : _ -> Yaml.value)]
  | { ptyp_desc = Ptyp_poly (names, typ); _ } ->
      polymorphic_function names (type_to_expr typ)
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
      let arg n = "arg" ^ string_of_int n in
      let tuple_pattern =
        Pat.tuple
          (List.mapi
             (fun i t -> Pat.var { loc = t.ptyp_loc; txt = arg i })
             typs)
      in
      let list_apps =
        [%expr
          `A
            [%e
              Ast_convenience.list
                (List.mapi
                   (fun i t ->
                     Ast_convenience.app (type_to_expr t)
                       [ Exp.ident (Ast_convenience.lid (arg i)) ])
                   typs)]]
      in
      [%expr fun [%p tuple_pattern] -> [%e list_apps]]
  | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type"

and function_app f l =
  if l = [] then f
  else Exp.apply f (List.map (fun e -> (Nolabel, e)) (List.map type_to_expr l))

and polymorphic_function names expr =
  List.fold_right
    (fun name expr ->
      let loc = name.Location.loc in
      let name = name.Location.txt in
      let arg = Pat.var { loc; txt = "poly_" ^ name } in
      [%expr fun [%p arg] -> [%e expr]])
    names expr

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
