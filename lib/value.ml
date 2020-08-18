open Ppxlib
open Ast_helper
open Ast_builder.Default

let arg n = "arg" ^ string_of_int n

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
          (Exp.ident (Helpers.mkloc (Helpers.mangle_suf Helpers.suf_to lid)))
          args
      in
      [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_var name; _ } ->
      let ident = Exp.ident (Located.lident ~loc ("poly_" ^ name)) in
      [%expr ([%e ident] : _ -> Yaml.value)]
  | { ptyp_desc = Ptyp_poly (names, typ); _ } ->
      polymorphic_function names (type_to_expr typ)
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
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
              Ast_builder.Default.elist ~loc
                (List.mapi
                   (fun i t ->
                     Ast_builder.Default.eapply ~loc (type_to_expr t)
                       [ Exp.ident (Located.lident ~loc (arg i)) ])
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
            (Located.lident ~loc pld_name.txt)
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

let wrap_open_rresult ~loc expr =
  [%expr
    let open! Rresult.R.Infix in
    [%e expr]]

let mk_pat_match ~loc cases =
  let cases = cases @ [ ([%pat? _], [%expr Error (`Msg "err")]) ] in
  Exp.function_ (List.map (fun (pat, exp) -> Exp.case pat exp) cases)

let rec of_yaml_type_to_expr name typ =
  let loc = typ.ptyp_loc in
  let argument, expr_arg =
    match name with
    | None -> (Pat.var { loc; txt = "x" }, Exp.ident (Located.lident ~loc "x"))
    | Some t -> (Pat.var { loc; txt = t }, Exp.ident (Located.lident ~loc t))
  in
  match typ with
  | [%type: int] ->
      mk_pat_match ~loc
        [
          ([%pat? `Float [%p argument]], [%expr Ok (int_of_float [%e expr_arg])]);
        ]
  | [%type: float] ->
      mk_pat_match ~loc
        [ ([%pat? `Float [%p argument]], [%expr Ok [%e expr_arg]]) ]
  | [%type: string] ->
      mk_pat_match ~loc
        [ ([%pat? `String [%p argument]], [%expr Ok [%e expr_arg]]) ]
  | [%type: bool] ->
      mk_pat_match ~loc
        [ ([%pat? `Bool [%p argument]], [%expr Ok [%e expr_arg]]) ]
  | [%type: char] ->
      mk_pat_match ~loc
        [ ([%pat? `String [%p argument]], [%expr Ok [%e expr_arg].[0]]) ]
  | [%type: [%t? typ] list] ->
      mk_pat_match ~loc
        [
          ( [%pat? `A lst],
            [%expr
              let open! Rresult.R.Infix in
              [%e Helpers.map_bind ~loc] [%e of_yaml_type_to_expr None typ] lst]
          );
        ]
  | [%type: [%t? typ] array] ->
      mk_pat_match ~loc
        [
          ( [%pat? `A lst],
            [%expr
              let open! Rresult.R.Infix in
              `A
                Array.(
                  to_list ([%e Helpers.map_bind ~loc] [%e type_to_expr typ]))]
          );
        ]
  | [%type: Yaml.value] -> [%expr fun x -> Ok x]
  | [%type: [%t? typ] option] ->
      mk_pat_match ~loc
        [
          ([%pat? None], [%expr Ok `Null]);
          ([%pat? Some t], [%expr Ok [%e of_yaml_type_to_expr None typ]]);
        ]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
      let fwd =
        function_appl
          (Exp.ident (Helpers.mkloc (Helpers.mangle_suf Helpers.suf_of lid)))
          args
      in
      [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_var name; _ } ->
      let ident = Exp.ident (Located.lident ~loc ("poly_" ^ name)) in
      [%expr ([%e ident] : Yaml.value -> _)]
  | { ptyp_desc = Ptyp_poly (names, typ); _ } ->
      polymorphic_function names (of_yaml_type_to_expr None typ)
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
      let list_pat =
        [%pat?
          `A
            [%p
              plist ~loc
                (List.mapi
                   (fun i t -> Pat.var { loc = t.ptyp_loc; txt = arg i })
                   typs)]]
      in
      let tup =
        [%expr
          Ok
            [%e
              Exp.tuple
                (List.mapi
                   (fun i t ->
                     [%expr [%e of_yaml_type_to_expr (Some (arg i)) t]])
                   typs)]]
      in
      mk_pat_match ~loc [ (list_pat, tup) ]
  | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type"

and function_appl f l =
  if l = [] then f
  else
    Exp.apply f
      (List.map
         (fun e -> (Nolabel, e))
         (List.map (of_yaml_type_to_expr None) l))

and polymorphic_function names expr =
  List.fold_right
    (fun name expr ->
      let loc = name.Location.loc in
      let name = name.Location.txt in
      let arg = Pat.var { loc; txt = "poly_" ^ name } in
      [%expr fun [%p arg] -> [%e expr]])
    names expr

(** Method used by PPX Deriving Yojson
    https://github.com/ocaml-ppx/ppx_deriving_yojson/blob/master/src/ppx_deriving_yojson.ml#L508
    The loop goes over the possible key-value pairs in the list and accumulates
    the possible values in a list. Once complete whatever the last value was is
    used in the construction of the record. *)
let of_yaml_record_to_expr ~loc fields =
  let monad_binding =
    List.fold_left (fun expr i ->
        let loc = expr.pexp_loc in
        [%expr
          [%e evar ~loc (arg i)] >>= fun [%p pvar ~loc (arg i)] -> [%e expr]])
  in
  let record =
    [%expr
      Ok
        [%e
          Exp.record
            (List.mapi
               (fun i f ->
                 (Located.lident ~loc f.pld_name.txt, evar ~loc (arg i)))
               fields)
            None]]
  in
  let base_case = monad_binding record (List.mapi (fun i _ -> i) fields) in
  let kv_cases =
    List.mapi
      (fun i f ->
        let funcs =
          List.mapi
            (fun j _ ->
              if i = j then
                eapply ~loc
                  (of_yaml_type_to_expr None f.pld_type)
                  [ evar ~loc "x" ]
              else evar ~loc (arg j))
            fields
        in
        Exp.case
          [%pat? ([%p pstring ~loc f.pld_name.txt], x) :: xs]
          [%expr loop xs [%e Helpers.etuple ~loc funcs]])
      fields
  in
  let kv_cases =
    kv_cases
    @ [
        Exp.case [%pat? []] base_case;
        Exp.case [%pat? _] [%expr Error (`Msg "Couldn't make record")];
      ]
  in
  let e =
    [%expr
      function
      | `O xs ->
          let rec loop xs
              ([%p
                 Helpers.ptuple ~loc
                   (List.mapi (fun i _ -> pvar ~loc (arg i)) fields)] as _state)
              =
            [%e Exp.match_ [%expr xs] kv_cases]
          in
          loop xs
            [%e
              Helpers.etuple ~loc
                (List.map
                   (fun _ ->
                     [%expr Error (`Msg "Didn't find the function for key")])
                   fields)]
      | _ -> Error (`Msg "Failed building a key-value object expecting a list")]
  in
  wrap_open_rresult ~loc e
