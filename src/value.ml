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
  | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } -> (
      let cases =
        let exception Failed_to_derive of location * string in
        try
          let l =
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
                | Rtag (label, _, _) ->
                    raise (Failed_to_derive (label.loc, "Rtag"))
                | Rinherit ctype ->
                    Exp.case
                      (Pat.variant ~loc "e" (Some (pvar ~loc "x")))
                      (type_to_expr ctype))
              row_fields
          in
          Ok l
        with Failed_to_derive (l, s) -> Error (`Msg (l, s))
      in
      match cases with
      | Error (`Msg (loc, m)) ->
          pexp_extension ~loc @@ Location.error_extensionf ~loc "%s" m
      | Ok cases -> Exp.function_ ~loc cases)
  | { ptyp_desc = Ptyp_arrow _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Functions cannot be converted yaml"
  | { ptyp_desc = Ptyp_object _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Objects cannot be converted yaml"
  | { ptyp_desc = Ptyp_class _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Classes cannot be converted yaml"
  | { ptyp_desc = Ptyp_any; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Any cannot be converted yaml"
  | { ptyp_desc = Ptyp_package _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Packages cannot be converted yaml"
  | { ptyp_desc = Ptyp_alias _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Aliases cannot be converted yaml"
  | { ptyp_desc = Ptyp_extension _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Extensions cannot be converted yaml"

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
        let func =
          match Attribute.get Attrs.to_yaml pld with
          | None -> type_to_expr pld_type
          | Some fn -> fn
        in
        let field =
          Exp.field
            (Ast_builder.Default.evar ~loc "x")
            (Located.lident ~loc pld_name.txt)
        in
        [%expr
          [%e
            match Attribute.get Attrs.default pld with
            | None ->
                [%expr
                  Some
                    ( [%e Ast_builder.Default.estring ~loc:pld_loc name],
                      [%e func] [%e field] )]
            | Some d ->
                [%expr
                  (fun x ->
                    if x = [%e d] then None
                    else
                      Some
                        ( [%e Ast_builder.Default.estring ~loc:pld_loc name],
                          [%e func] x ))
                    [%e field]]]])
      fs
  in
  let fs = fields_to_expr fields in
  [%expr
    fun (x : [%t typ]) ->
      `O (Stdlib.List.filter_map (fun x -> x) [%e Ast_builder.Default.elist ~loc fs])]

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

let mk_pat_match ~loc cases typ =
  let cases =
    cases
    @ [
        ( [%pat? _],
          [%expr
            Error
              (`Msg
                [%e
                  estring ~loc
                    ("Was expecting '" ^ typ ^ "' but got a different type")])]
        );
      ]
  in
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
        "int"
  | [%type: float] ->
      mk_pat_match ~loc
        [ ([%pat? `Float [%p argument]], [%expr Ok [%e expr_arg]]) ]
        "float"
  | [%type: string] ->
      mk_pat_match ~loc
        [ ([%pat? `String [%p argument]], [%expr Ok [%e expr_arg]]) ]
        "string"
  | [%type: bool] ->
      mk_pat_match ~loc
        [ ([%pat? `Bool [%p argument]], [%expr Ok [%e expr_arg]]) ]
        "bool"
  | [%type: char] ->
      mk_pat_match ~loc
        [ ([%pat? `String [%p argument]], [%expr Ok [%e expr_arg].[0]]) ]
        "char"
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
        "list"
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
        "array"
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
            Stdlib.Result.Ok
              [%e
                Helpers.etuple ~loc
                  (List.mapi (fun i _ -> evar ~loc (arg i)) typs)]]
          funcs
      in
      wrap_open_rresult ~loc (mk_pat_match ~loc [ (list_pat, expr) ] "null")
  | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      let cases =
        List.map
          (fun field ->
            match field.prf_desc with
            | Rtag (name, true, []) ->
                Exp.case
                  [%pat? `O [ ([%p pstring ~loc name.txt], `A []) ]]
                  [%expr Stdlib.Result.Ok [%e Exp.variant name.txt None]]
            | Rtag (name, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
                let e =
                  monad_fold
                    (of_yaml_type_to_expr None)
                    [%expr
                      Stdlib.Result.Ok
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
                    Stdlib.Result.Ok [%e Exp.variant name.txt (Some (evar ~loc "x"))]]
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
  | { ptyp_desc = Ptyp_arrow _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Functions cannot be converted yaml"
  | { ptyp_desc = Ptyp_object _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Objects cannot be converted yaml"
  | { ptyp_desc = Ptyp_class _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Classes cannot be converted yaml"
  | { ptyp_desc = Ptyp_any; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Any cannot be converted yaml"
  | { ptyp_desc = Ptyp_package _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Packages cannot be converted yaml"
  | { ptyp_desc = Ptyp_alias _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Aliases cannot be converted yaml"
  | { ptyp_desc = Ptyp_extension _; _ } ->
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc "Extensions cannot be converted yaml"

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

let of_yaml_record_to_expr ~loc ~skip_unknown fields =
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
                match Attribute.get Attrs.of_yaml f with
                | Some fn -> eapply ~loc fn [ evar ~loc "x" ]
                | None ->
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
        (if skip_unknown then Exp.case [%pat? _ :: xs] [%expr loop xs _state]
         else
           Exp.case
             [%pat? (x, y) :: _]
             [%expr Error (`Msg (x ^ Yaml.to_string_exn y))]);
      ]
  in
  let option_to_none t =
    match Attribute.get Attrs.default t with
    | None -> (
        match t.pld_type with
        | [%type: [%t? _] option] -> [%expr Ok None]
        | _ ->
            [%expr
              Error
                (`Msg
                  [%e
                    estring ~loc
                      ("Didn't find the function for key: " ^ t.pld_name.txt)])]
        )
    | Some default -> [%expr Ok [%e default]]
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
