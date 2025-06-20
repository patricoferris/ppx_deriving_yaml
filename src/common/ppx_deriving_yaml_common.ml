open Ppxlib
open Ast_helper
open Ast_builder.Default

module Helpers = struct
  let arg n = "arg" ^ string_of_int n
  let mkloc txt = { txt; loc = !Ast_helper.default_loc }

  let fold_right f type_decl acc =
    let fold f params acc =
      List.fold_right
        (fun (p, _) acc ->
          match p with
          | { ptyp_desc = Ptyp_any; _ } -> acc
          | { ptyp_desc = Ptyp_var name; _ } ->
              let name = { txt = name; loc = p.ptyp_loc } in
              f name acc
          | _ -> assert false)
        params acc
    in
    fold f type_decl.ptype_params acc

  let poly_fun ~loc typ_decl expr =
    fold_right
      (fun name expr ->
        let name = name.txt in
        Exp.fun_ Nolabel None
          (Ast_helper.Pat.var { loc; txt = "poly_" ^ name })
          expr)
      typ_decl expr

  let ptuple ~loc = function
    | [] -> [%pat? ()]
    | [ x ] -> x
    | xs -> Pat.tuple ~loc xs

  let etuple ~loc = function
    | [] -> [%expr ()]
    | [ x ] -> x
    | xs -> Exp.tuple ~loc xs

  let add_suffix ?(fixpoint = "t") suf lid =
    match lid with
    | (Lident t | Ldot (_, t)) when t = fixpoint -> suf
    | Lident t | Ldot (_, t) -> t ^ "_" ^ suf
    | Lapply _ -> assert false

  let mangle_suf ?fixpoint suf lid =
    match lid with
    | Lident _t -> Lident (add_suffix ?fixpoint suf lid)
    | Ldot (p, _t) -> Ldot (p, add_suffix ?fixpoint suf lid)
    | Lapply _ -> assert false

  let map_bind ~loc =
    [%expr
      fun f lst ->
        Stdlib.List.fold_left
          (fun acc x ->
            match acc with
            | Stdlib.Ok acc -> f x >>= fun x -> Stdlib.Ok (x :: acc)
            | Stdlib.Error e -> Stdlib.Error e)
          (Stdlib.Ok []) lst
        >>= fun lst -> Stdlib.Ok (Stdlib.List.rev lst)]
end

let arg = Helpers.arg

exception Failed_to_derive of location * string

module type Backend = sig
  val backend : string
  val typename : string
  val suf_to : string
  val suf_of : string

  module Attrs : sig
    val key : (label_declaration, label) Attribute.t
    val name : (constructor_declaration, string) Attribute.t
    val default : (label_declaration, expression) Attribute.t
    val to_ : (label_declaration, expression) Attribute.t
    val of_ : (label_declaration, expression) Attribute.t
  end
end

module Make (B : Backend) = struct
  let backend_constructor =
    Typ.constr (Located.lident ~loc:Location.none B.typename) []

  let polymorphic_msg =
    let msg =
      Rtag
        ( { txt = "Msg"; loc = Location.none },
          false,
          [ Typ.constr (Located.lident ~loc:Location.none "string") [] ] )
    in
    let msg_row =
      { prf_desc = msg; prf_attributes = []; prf_loc = Location.none }
    in
    Typ.variant [ msg_row ] Open None

  let arrow_backend ?result ~loc arg =
    let return =
      match result with
      | Some r ->
          Typ.constr (Located.lident ~loc "result") [ r; polymorphic_msg ]
      | None -> backend_constructor
    in
    Typ.arrow Nolabel arg return

  let function_returning_backend ?result ~loc expr arg =
    Exp.constraint_ expr (arrow_backend ?result ~loc arg)

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
    | [%type: [%t? typ] option] ->
        [%expr function None -> `Null | Some t -> [%e type_to_expr typ] t]
    (* When Yaml.value or Ezjsonm.value is found in the type declaration *)
    | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, []); _ }
      when Longident.name lid = B.typename ->
        [%expr fun x -> x]
    | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
        let fwd =
          function_app
            (Exp.ident (Helpers.mkloc (Helpers.mangle_suf B.suf_to lid)))
            args
        in
        [%expr fun x -> [%e fwd] x]
    | { ptyp_desc = Ptyp_var name; _ } ->
        let ident = Exp.ident (Located.lident ~loc ("poly_" ^ name)) in
        function_returning_backend ~loc ident (Typ.any ())
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
                  | Rtag (label, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ])
                    ->
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
    | { ptyp_desc = Ptyp_open (module_e, typ); _ } ->
        let module_ident = Opn.mk (Mod.mk (Pmod_ident module_e)) in
        let v = type_to_expr typ in
        pexp_open ~loc module_ident v
    | { ptyp_desc = Ptyp_arrow _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Functions cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_object _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Objects cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_class _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Classes cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_any; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Any cannot be converted %s" B.backend
    | { ptyp_desc = Ptyp_package _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Packages cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_alias _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Aliases cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_extension _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Extensions cannot be converted %s"
             B.backend

  and function_app f l =
    if l = [] then f
    else
      Exp.apply f (List.map (fun e -> (Nolabel, e)) (List.map type_to_expr l))

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
            Option.value ~default:pld_name.txt (Attribute.get B.Attrs.key pld)
          in
          let func =
            match Attribute.get B.Attrs.to_ pld with
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
              match Attribute.get B.Attrs.default pld with
              | None ->
                  [%expr
                    Some
                      ( [%e Ast_builder.Default.estring ~loc:pld_loc name],
                        [%e func] [%e field] )]
              | Some d ->
                  [%expr
                    (fun x ->
                      if Stdlib.( = ) x [%e d] then None
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
        `O
          (Stdlib.List.filter_map
             (fun x -> x)
             [%e Ast_builder.Default.elist ~loc fs])]

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
            let func_t = arrow_backend ~loc arg in
            [%type: [%t func_t] -> [%t typ]]
        | _ -> assert false)
      type_decl.ptype_params (arrow_backend ~loc t)

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
            let func_t = arrow_backend ~result:arg ~loc backend_constructor in
            [%type: [%t func_t] -> [%t typ]]
        | _ -> assert false)
      type_decl.ptype_params
      (arrow_backend ~result:t ~loc backend_constructor)

  let wrap_open_rresult ~loc expr =
    [%expr
      let[@warning "-26"] ( >>= ) v f =
        match v with Ok v -> f v | Error _ as e -> e
      in
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

  let rec of_backend_type_to_expr name typ =
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
            ( [%pat? `Float [%p argument]],
              [%expr Ok (int_of_float [%e expr_arg])] );
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
                [%e Helpers.map_bind ~loc]
                  [%e of_backend_type_to_expr None typ]
                  lst] );
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
    | [%type: [%t? typ] option] ->
        [%expr
          function
          | `Null -> Ok None
          | x ->
              [%e of_backend_type_to_expr None typ] x >>= fun x -> Ok (Some x)]
    | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, []); _ }
      when Longident.name lid = B.typename ->
        [%expr fun x -> Ok x]
    | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
        let fwd =
          function_appl
            (Exp.ident (Helpers.mkloc (Helpers.mangle_suf B.suf_of lid)))
            args
        in
        [%expr fun x -> [%e fwd] x]
    | { ptyp_desc = Ptyp_var name; _ } ->
        let ident = Exp.ident (Located.lident ~loc ("poly_" ^ name)) in
        [%expr
          ([%e ident]
            : [%t backend_constructor] -> (_, [> `Msg of string ]) result)]
    | { ptyp_desc = Ptyp_poly (names, typ); _ } ->
        polymorphic_function names (of_backend_type_to_expr None typ)
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
            (fun i t ->
              (i, [%expr [%e of_backend_type_to_expr (Some (arg i)) t]]))
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
                      (of_backend_type_to_expr None)
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
                                  (List.mapi
                                     (fun i _ -> pvar ~loc (arg i))
                                     typs)] );
                        ]]
                    e
              | Rtag (name, false, [ t ]) ->
                  Exp.case
                    [%pat? `O [ ([%p pstring ~loc name.txt], `A [ x ]) ]]
                    [%expr
                      [%e of_backend_type_to_expr None t] x >>= fun x ->
                      Stdlib.Result.Ok
                        [%e Exp.variant name.txt (Some (evar ~loc "x"))]]
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
    | { ptyp_desc = Ptyp_open (module_e, typ); _ } ->
        let module_ident = Opn.mk (Mod.mk (Pmod_ident module_e)) in
        let v = of_backend_type_to_expr name typ in
        pexp_open ~loc module_ident v
    | { ptyp_desc = Ptyp_arrow _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Functions cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_object _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Objects cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_class _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Classes cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_any; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Any cannot be converted %s" B.backend
    | { ptyp_desc = Ptyp_package _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Packages cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_alias _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Aliases cannot be converted %s"
             B.backend
    | { ptyp_desc = Ptyp_extension _; _ } ->
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc "Extensions cannot be converted %s"
             B.backend

  and function_appl f l =
    if l = [] then f
    else
      Exp.apply f
        (List.map
           (fun e -> (Nolabel, e))
           (List.map (of_backend_type_to_expr None) l))

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
      The loop goes over the possible key-value pairs in the list and
      accumulates the possible values in a list. Once complete whatever the last
      value was is used in the construction of the record. *)

  let of_backend_record_to_expr ~loc ~skip_unknown fields =
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
            Option.value ~default:f.pld_name.txt (Attribute.get B.Attrs.key f)
          in
          let funcs =
            List.mapi
              (fun j _ ->
                if i = j then
                  match Attribute.get B.Attrs.of_ f with
                  | Some fn -> eapply ~loc fn [ evar ~loc "x" ]
                  | None ->
                      eapply ~loc
                        (of_backend_type_to_expr None f.pld_type)
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
               [%pat? (x, _y) :: _]
               [%expr Error (`Msg ("Failed to find the case for: " ^ x))]);
        ]
    in
    let option_to_none t =
      match Attribute.get B.Attrs.default t with
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
                     (List.mapi (fun i _ -> pvar ~loc (arg i)) fields)] as
                 _state) =
              [%e Exp.match_ [%expr xs] kv_cases]
            in
            loop xs
              [%e
                Helpers.etuple ~loc
                  (List.map (fun f -> option_to_none f) fields)]
        | _ ->
            Error (`Msg "Failed building a key-value object expecting a list")]
    in
    wrap_open_rresult ~loc e

  (* Higher level constructions *)

  let failed_to_derive loc msg = raise (Failed_to_derive (loc, msg))

  let mangle_name_label suff label =
    if label = "t" then suff else label ^ "_" ^ suff

  (* We need to check if a type is recursive or not in it's definition *)
  let check_rec_type rec_flag typ =
    let check =
      object
        inherit type_is_recursive rec_flag typ
      end
    in
    check#go

  let generate_impl_of ~ctxt (rec_flag, type_decls) skip_unknown =
    let rec_flag = check_rec_type rec_flag type_decls () in
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.concat
      (List.map
         (fun typ_decl ->
           match typ_decl with
           | { ptype_kind = Ptype_abstract; ptype_manifest; ptype_name; _ } -> (
               match ptype_manifest with
               | Some t ->
                   let ocamliser =
                     Helpers.poly_fun ~loc:typ_decl.ptype_loc typ_decl
                       (of_backend_type_to_expr None t)
                   in
                   let of_yaml = mangle_name_label B.suf_of ptype_name.txt in
                   [
                     pstr_value ~loc rec_flag
                       [
                         Vb.mk (ppat_var ~loc { loc; txt = of_yaml }) ocamliser;
                       ];
                   ]
               | None ->
                   [
                     pstr_value ~loc rec_flag
                       [
                         Vb.mk
                           (ppat_var ~loc { loc; txt = "error_encountered" })
                           (pexp_extension ~loc
                           @@ Location.error_extensionf ~loc
                                "Failed to derive something for an abstract \
                                 type with no manifest!");
                       ];
                   ])
           | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
               let of_yaml = mangle_name_label B.suf_of ptype_name.txt in
               let of_yaml_cases =
                 let l =
                   List.map
                     (fun ({ pcd_name; pcd_args; _ } as p) ->
                       let name =
                         Option.value ~default:pcd_name.txt
                           (Attribute.get B.Attrs.name p)
                       in
                       match pcd_args with
                       | Pcstr_tuple args ->
                           let tuple =
                             if List.length args = 0 then None
                             else
                               Some
                                 (Helpers.etuple ~loc
                                    (List.mapi
                                       (fun i _ -> evar ~loc (Helpers.arg i))
                                       args))
                           in
                           Exp.case
                             [%pat?
                               `O
                                 [
                                   ( [%p pstring ~loc name],
                                     `A
                                       [%p
                                         plist ~loc
                                           (List.mapi
                                              (fun i _ ->
                                                pvar ~loc (Helpers.arg i))
                                              args)] );
                                 ]]
                             (monad_fold
                                (of_backend_type_to_expr None)
                                [%expr
                                  Stdlib.Result.Ok
                                    [%e
                                      Exp.construct
                                        {
                                          txt = Lident pcd_name.txt;
                                          loc = pcd_name.loc;
                                        }
                                        tuple]]
                                (List.mapi (fun i t -> (t, i)) args))
                       | _ -> failed_to_derive loc "Failed to derive variant")
                     constructors
                 in
                 Ok l
               in
               let of_yaml_cases =
                 match of_yaml_cases with
                 | Error _ as e -> e
                 | Ok cases ->
                     Ok
                       (cases
                       @ [
                           Exp.case
                             [%pat? _]
                             [%expr
                               Stdlib.Error
                                 (`Msg "no match for this variant expression")];
                         ])
               in
               let of_yaml_expr =
                 match of_yaml_cases with
                 | Error (loc, msg) ->
                     pexp_extension ~loc
                     @@ Location.error_extensionf ~loc "%s" msg
                 | Ok of_yaml_cases ->
                     wrap_open_rresult ~loc (Exp.function_ ~loc of_yaml_cases)
               in
               [
                 pstr_value ~loc rec_flag
                   [ Vb.mk (ppat_var ~loc { loc; txt = of_yaml }) of_yaml_expr ];
               ]
           | { ptype_kind = Ptype_record fields; ptype_loc; ptype_name; _ } ->
               let of_yaml = mangle_name_label B.suf_of ptype_name.txt in
               [
                 pstr_value ~loc rec_flag
                   [
                     Vb.mk
                       (ppat_var ~loc { loc; txt = of_yaml })
                       (Helpers.poly_fun ~loc:ptype_loc typ_decl
                          (of_backend_record_to_expr ~skip_unknown
                             ~loc:ptype_loc fields));
                   ];
               ]
           | _ ->
               [
                 pstr_value ~loc rec_flag
                   [
                     Vb.mk (ppat_var ~loc { loc; txt = "error" })
                     @@ pexp_extension ~loc
                     @@ Location.error_extensionf ~loc
                          "Cannot derive anything for this type";
                   ];
               ])
         type_decls)

  let vb_error loc msg =
    [
      pstr_value ~loc Nonrecursive
        [
          Vb.mk
            (ppat_var ~loc { loc; txt = "error_encountered" })
            (pexp_extension ~loc @@ Location.error_extensionf ~loc "%s" msg);
        ];
    ]

  let generate_impl_to ~ctxt (rec_flag, type_decls) =
    let rec_flag = check_rec_type rec_flag type_decls () in
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.concat
      (List.map
         (fun typ_decl ->
           match typ_decl with
           | { ptype_kind = Ptype_abstract; ptype_manifest; ptype_name; _ } -> (
               match ptype_manifest with
               | Some t ->
                   let yamliser =
                     Helpers.poly_fun ~loc:typ_decl.ptype_loc typ_decl
                       (type_to_expr t)
                   in
                   let to_yaml = mangle_name_label B.suf_to ptype_name.txt in
                   [
                     pstr_value ~loc rec_flag
                       [ Vb.mk (ppat_var ~loc { loc; txt = to_yaml }) yamliser ];
                   ]
               | None -> vb_error loc "Cannot derive anything for this type")
           | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
               let to_yaml = mangle_name_label B.suf_to ptype_name.txt in
               let to_yaml_cases =
                 try
                   let l =
                     List.map
                       (fun ({ pcd_name; pcd_args; _ } as p) ->
                         let name =
                           Option.value ~default:pcd_name.txt
                             (Attribute.get B.Attrs.name p)
                         in
                         match pcd_args with
                         | Pcstr_tuple args ->
                             let pat_arg =
                               if List.length args = 0 then None
                               else
                                 Some
                                   (Helpers.ptuple ~loc
                                      (List.mapi
                                         (fun i _ -> pvar ~loc (Helpers.arg i))
                                         args))
                             in
                             Exp.case (pconstruct p pat_arg)
                               [%expr
                                 `O
                                   [
                                     ( [%e estring ~loc name],
                                       `A
                                         [%e
                                           elist ~loc
                                             (List.mapi
                                                (fun i t ->
                                                  [%expr
                                                    [%e type_to_expr t]
                                                      [%e
                                                        evar ~loc
                                                          (Helpers.arg i)]])
                                                args)] );
                                   ]]
                         | _ -> failwith "Not implemented!")
                       constructors
                   in
                   Ok l
                 with Failed_to_derive (loc, msg) -> Error (`Msg (loc, msg))
               in
               let to_yaml_expr =
                 match to_yaml_cases with
                 | Error (`Msg (loc, msg)) ->
                     pexp_extension ~loc
                     @@ Location.error_extensionf ~loc "%s" msg
                 | Ok to_yaml_cases -> Exp.function_ ~loc to_yaml_cases
               in
               [
                 pstr_value ~loc rec_flag
                   [ Vb.mk (ppat_var ~loc { loc; txt = to_yaml }) to_yaml_expr ];
               ]
           | { ptype_kind = Ptype_record fields; ptype_loc; ptype_name; _ } ->
               let to_yaml = mangle_name_label B.suf_to ptype_name.txt in
               [
                 pstr_value ~loc rec_flag
                   [
                     Vb.mk
                       [%pat? [%p ppat_var ~loc { loc; txt = to_yaml }]]
                       [%expr
                         [%e
                           Helpers.poly_fun ~loc:ptype_loc typ_decl
                             (record_to_expr
                                ~typ:(core_type_of_type_declaration typ_decl)
                                ~loc:ptype_loc fields)]];
                   ];
               ]
           | _ -> vb_error loc "Cannot derive anything for this type")
         type_decls)

  let generate_intf_to ~ctxt (_rec_flag, type_decls) :
      Ppxlib.Ast.signature_item list =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.map
      (fun typ_decl ->
        match typ_decl with
        | { ptype_kind = Ptype_abstract | Ptype_record _; _ } ->
            [
              psig_value ~loc
                (Val.mk
                   {
                     loc = typ_decl.ptype_name.loc;
                     txt = mangle_name_label B.suf_to typ_decl.ptype_name.txt;
                   }
                   (type_decl_to_type typ_decl));
            ]
        | _ ->
            [
              psig_value ~loc
                (Val.mk
                   { loc; txt = "error_encountered" }
                   (ptyp_extension ~loc
                   @@ Location.error_extensionf ~loc
                        "Cannot derived\n          anything"));
            ])
      type_decls
    |> List.concat

  let generate_intf_of ~ctxt (_rec_flag, type_decls) :
      Ppxlib.Ast.signature_item list =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.map
      (fun typ_decl ->
        match typ_decl with
        | { ptype_kind = Ptype_abstract | Ptype_record _; _ } ->
            [
              psig_value ~loc
                (Val.mk
                   {
                     loc = typ_decl.ptype_name.loc;
                     txt = mangle_name_label B.suf_of typ_decl.ptype_name.txt;
                   }
                   (type_decl_of_type typ_decl));
            ]
        | _ ->
            [
              psig_value ~loc
                (Val.mk
                   { loc; txt = "error_encountered" }
                   (ptyp_extension ~loc
                   @@ Location.error_extensionf ~loc
                        "Cannot derived\n          anything"));
            ])
      type_decls
    |> List.concat

  let impl_generator_to impl =
    let open B in
    Deriving.Generator.V2.make_noarg
      ~attributes:
        [
          Attribute.T Attrs.default;
          Attribute.T Attrs.name;
          Attribute.T Attrs.key;
        ]
      impl

  let impl_generator_of impl =
    let open B in
    Deriving.Generator.V2.make
      ~attributes:
        [
          Attribute.T Attrs.default;
          Attribute.T Attrs.name;
          Attribute.T Attrs.key;
        ]
      Deriving.Args.(empty +> flag "skip_unknown")
      impl
end
