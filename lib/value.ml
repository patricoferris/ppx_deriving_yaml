open Ppxlib
open Ast_helper
open Ast_builder.Default

let arg = Helpers.arg

(* to_yaml *)
let rec type_to_expr typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: int] -> [%expr fun (x : int) -> `Float (float_of_int x)]
  | [%type: float] -> [%expr fun (x : float) -> `Float x]
  | [%type: string] -> [%expr fun (x : string) -> `String x]
  | [%type: bool] -> [%expr fun (x : bool) -> `Bool x]
  | [%type: char] -> [%expr fun (x : char) -> `String (String.make 1 x)]
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
  | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      let cases =
        List.map
          (fun (field : row_field) ->
            match field.prf_desc with
            | Rtag (label, true, []) ->
                Exp.case
                  (Pat.variant label.txt None)
                  [%expr `O [ ([%e estring ~loc label.txt], `A []) ]]
            | Rtag (label, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
                Exp.case
                  (Pat.variant label.txt
                     (Some
                        (Helpers.ptuple ~loc
                           (List.mapi (fun i _ -> pvar ~loc (arg i)) typs))))
                  [%expr
                    `O
                      [
                        ( [%e estring ~loc label.txt],
                          `A
                            [%e
                              elist ~loc
                                (List.mapi
                                   (fun i t ->
                                     [%expr
                                       [%e type_to_expr t]
                                         [%e evar ~loc (arg i)]])
                                   typs)] );
                      ]]
            | Rtag (label, false, [ t ]) ->
                Exp.case
                  (Pat.variant ~loc label.txt (Some (pvar ~loc "x")))
                  [%expr
                    [%e type_to_expr t] [%e evar ~loc "x"] |> fun x ->
                    `O [ ([%e estring ~loc label.txt], `A [ x ]) ]]
            | _ -> failwith "Not implemented")
          row_fields
      in
      Exp.function_ ~loc cases
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

let record_to_expr ~typ ~loc fields =
  let fields_to_expr fs =
    List.map
      (fun ({ pld_name; pld_type; pld_loc; _ } as pld) ->
        let name =
          Option.value ~default:pld_name.txt (Attribute.get Attrs.key pld)
        in
        let field =
          Exp.field
            (Ast_builder.Default.evar ~loc "x")
            (Located.lident ~loc pld_name.txt)
        in
        [%expr
          [%e Ast_builder.Default.estring ~loc:pld_loc name],
            [%e type_to_expr pld_type] [%e field]])
      fs
  in
  let fs = fields_to_expr fields in
  [%expr fun (x : [%t typ]) -> `O [%e Ast_builder.Default.elist ~loc fs]]

let type_decl_to_type type_decl =
  let loc = type_decl.ptype_loc in
  let t = core_type_of_type_declaration type_decl in
  List.fold_right
    (fun (param, _) typ ->
      match param.ptyp_desc with
      | Ptyp_any -> typ
      | Ptyp_var name ->
          let loc = param.ptyp_loc in
          let arg = Typ.var ~loc name in
          [%type: ([%t arg] -> Yaml.value) -> [%t typ]]
      | _ -> assert false)
    type_decl.ptype_params [%type: [%t t] -> Yaml.value]

let type_decl_of_type type_decl =
  let loc = type_decl.ptype_loc in
  let t = core_type_of_type_declaration type_decl in
  List.fold_right
    (fun (param, _) typ ->
      match param.ptyp_desc with
      | Ptyp_any -> typ
      | Ptyp_var name ->
          let loc = param.ptyp_loc in
          let arg = Typ.var ~loc name in
          [%type: (Yaml.value -> [%t arg] Yaml.res) -> [%t typ]]
      | _ -> assert false)
    type_decl.ptype_params [%type: Yaml.value -> [%t t] Yaml.res]

let wrap_open_rresult ~loc expr =
  [%expr
    let ( >>= ) v f = match v with Ok v -> f v | Error _ as e -> e in
    [%e expr]]

let mk_pat_match ~loc cases =
  let cases = cases @ [ ([%pat? _], [%expr Error (`Msg "err")]) ] in
  Exp.function_ (List.map (fun (pat, exp) -> Exp.case pat exp) cases)

let monad_fold f =
  List.fold_left (fun expr (t, i) ->
      let loc = expr.pexp_loc in
      [%expr
        [%e f t] [%e evar ~loc (arg i)] >>= fun [%p pvar ~loc (arg i)] ->
        [%e expr]])

(* of_yaml *)
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
              let ( >>= ) v f =
                match v with Ok v -> f v | Error _ as e -> e
              in
              [%e Helpers.map_bind ~loc] [%e of_yaml_type_to_expr None typ] lst]
          );
        ]
  | [%type: [%t? typ] array] ->
      mk_pat_match ~loc
        [
          ( [%pat? `A lst],
            [%expr
              let ( >>= ) v f =
                match v with Ok v -> f v | Error _ as e -> e
              in
              `A
                Array.(
                  to_list ([%e Helpers.map_bind ~loc] [%e type_to_expr typ]))]
          );
        ]
  | [%type: Yaml.value] -> [%expr fun x -> Ok x]
  | [%type: [%t? typ] option] ->
      [%expr
        function
        | `Null -> Ok None
        | x -> [%e of_yaml_type_to_expr None typ] x >>= fun x -> Ok (Some x)]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
      let fwd =
        function_appl
          (Exp.ident (Helpers.mkloc (Helpers.mangle_suf Helpers.suf_of lid)))
          args
      in
      [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_var name; _ } ->
      let ident = Exp.ident (Located.lident ~loc ("poly_" ^ name)) in
      [%expr ([%e ident] : Yaml.value -> (_, [> `Msg of string ]) result)]
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
      let funcs =
        List.mapi
          (fun i t -> (i, [%expr [%e of_yaml_type_to_expr (Some (arg i)) t]]))
          typs
      in
      let expr =
        List.fold_left
          (fun acc (i, t) ->
            [%expr
              [%e t] [%e evar ~loc (arg i)] >>= fun [%p pvar ~loc (arg i)] ->
              [%e acc]])
          [%expr
            Result.Ok
              [%e
                Helpers.etuple ~loc
                  (List.mapi (fun i _ -> evar ~loc (arg i)) typs)]]
          funcs
      in
      wrap_open_rresult ~loc (mk_pat_match ~loc [ (list_pat, expr) ])
  | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      let cases =
        List.map
          (fun field ->
            match field.prf_desc with
            | Rtag (name, true, []) ->
                Exp.case
                  [%pat? `O [ ([%p pstring ~loc name.txt], `A []) ]]
                  [%expr Result.Ok [%e Exp.variant name.txt None]]
            | Rtag (name, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
                let e =
                  monad_fold
                    (of_yaml_type_to_expr None)
                    [%expr
                      Result.Ok
                        [%e
                          Exp.variant name.txt
                            (Some
                               (Helpers.etuple ~loc
                                  (List.mapi
                                     (fun i _ -> evar ~loc (arg i))
                                     typs)))]]
                    (List.mapi (fun i t -> (t, i)) typs)
                in
                Exp.case
                  [%pat?
                    `O
                      [
                        ( [%p pstring ~loc name.txt],
                          `A
                            [%p
                              plist ~loc
                                (List.mapi (fun i _ -> pvar ~loc (arg i)) typs)]
                        );
                      ]]
                  e
            | Rtag (name, false, [ t ]) ->
                Exp.case
                  [%pat? `O [ ([%p pstring ~loc name.txt], `A [ x ]) ]]
                  [%expr
                    [%e of_yaml_type_to_expr None t] x >>= fun x ->
                    Result.Ok [%e Exp.variant name.txt (Some (evar ~loc "x"))]]
            | _ -> Exp.case [%pat? _] [%expr Error (`Msg "Not implemented")])
          row_fields
      in
      wrap_open_rresult ~loc
        (Exp.function_ ~loc
           (cases
           @ [
               Exp.case
                 [%pat? _]
                 [%expr Error (`Msg "failed converting variant")];
             ]))
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

let derive_pattern ~loc label_decl =
      let { pld_name; pld_type; _ } = label_decl in
      let default = (Attribute.get Attrs.default label_decl) in 
         match default, pld_type with
         | Some default, _ -> Optional pld_name.txt, pvar ~loc pld_name.txt, Some default
         | _ , [%type: [%t? _] list] -> Optional pld_name.txt, pvar ~loc pld_name.txt, Some (elist ~loc [])
         | _, [%type: [%t? _] option] -> Optional pld_name.txt, pvar ~loc pld_name.txt, None
         | None,  _ -> Labelled pld_name.txt, pvar ~loc pld_name.txt, None

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
        let name =
          Option.value ~default:f.pld_name.txt (Attribute.get Attrs.key f)
        in
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
          [%pat? ([%p pstring ~loc name], x) :: xs]
          [%expr loop xs [%e Helpers.etuple ~loc funcs]])
      fields
  in
  let kv_cases =
    kv_cases
    @ [
        Exp.case [%pat? []] base_case;
        Exp.case
          [%pat? (x, y) :: _]
          [%expr Error (`Msg (x ^ Yaml.to_string_exn y))];
      ]
  in
  let patterns = List.map (derive_pattern ~loc) fields in
  let option_to_none t =
    match t.pld_type with
    | [%type: [%t? _] option] -> [%expr Ok None]
    | _ ->
        [%expr
          Error
            (`Msg
              [%e
                estring ~loc
                  ("Didn't find the function for key: " ^ t.pld_name.txt)])]
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
              Helpers.etuple ~loc (List.map (fun f -> option_to_none f) fields)]
      | _ -> Error (`Msg "Failed building a key-value object expecting a list")]
  in
  wrap_open_rresult ~loc e
