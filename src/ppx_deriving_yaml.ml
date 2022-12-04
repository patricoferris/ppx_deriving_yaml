open Ppxlib
open Ast_helper
open Ast_builder.Default

let suf_to = Helpers.suf_to
let suf_of = Helpers.suf_of

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

let generate_impl_of_yaml ~ctxt (rec_flag, type_decls) skip_unknown =
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
                     (Value.of_yaml_type_to_expr None t)
                 in
                 let of_yaml = mangle_name_label suf_of ptype_name.txt in
                 [
                   pstr_value ~loc rec_flag
                     [ Vb.mk (ppat_var ~loc { loc; txt = of_yaml }) ocamliser ];
                 ]
             | None ->
                 Location.raise_errorf ~loc
                   "Cannot derive anything for this type")
         | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
             let of_yaml = mangle_name_label suf_of ptype_name.txt in
             let of_yaml_cases =
               List.map
                 (fun ({ pcd_name; pcd_args; _ } as p) ->
                   let name =
                     Option.value ~default:pcd_name.txt
                       (Attribute.get Attrs.name p)
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
                                          (fun i _ -> pvar ~loc (Helpers.arg i))
                                          args)] );
                             ]]
                         Value.(
                           monad_fold
                             (of_yaml_type_to_expr None)
                             [%expr
                               Result.Ok
                                 [%e
                                   Exp.construct
                                     {
                                       txt = Lident pcd_name.txt;
                                       loc = pcd_name.loc;
                                     }
                                     tuple]]
                             (List.mapi (fun i t -> (t, i)) args))
                   | _ -> failwith "Not implemented!")
                 constructors
             in
             let of_yaml_cases =
               of_yaml_cases
               @ [
                   Exp.case
                     [%pat? _]
                     [%expr Error (`Msg "no match for this variant expression")];
                 ]
             in
             let of_yaml_expr =
               Value.wrap_open_rresult ~loc (Exp.function_ ~loc of_yaml_cases)
             in
             [
               pstr_value ~loc rec_flag
                 [ Vb.mk (ppat_var ~loc { loc; txt = of_yaml }) of_yaml_expr ];
             ]
         | { ptype_kind = Ptype_record fields; ptype_loc; ptype_name; _ } ->
             let of_yaml = mangle_name_label suf_of ptype_name.txt in
             [
               pstr_value ~loc rec_flag
                 [
                   Vb.mk
                     (ppat_var ~loc { loc; txt = of_yaml })
                     (Helpers.poly_fun ~loc:ptype_loc typ_decl
                        (Value.of_yaml_record_to_expr ~skip_unknown
                           ~loc:ptype_loc fields));
                 ];
             ]
         | _ ->
             Location.raise_errorf ~loc "Cannot derive anything for this type")
       type_decls)

let generate_impl_to_yaml ~ctxt (rec_flag, type_decls) =
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
                     [%expr [%e Value.type_to_expr t]]
                 in
                 let to_yaml = mangle_name_label suf_to ptype_name.txt in
                 [
                   pstr_value ~loc rec_flag
                     [ Vb.mk (ppat_var ~loc { loc; txt = to_yaml }) yamliser ];
                 ]
             | None ->
                 Location.raise_errorf ~loc
                   "Cannot derive anything for this type")
         | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
             let to_yaml = mangle_name_label suf_to ptype_name.txt in
             let to_yaml_cases =
               List.map
                 (fun ({ pcd_name; pcd_args; _ } as p) ->
                   let name =
                     Option.value ~default:pcd_name.txt
                       (Attribute.get Attrs.name p)
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
                                              [%e Value.type_to_expr t]
                                                [%e evar ~loc (Helpers.arg i)]])
                                          args)] );
                             ]]
                   | _ -> failwith "Not implemented!")
                 constructors
             in
             let to_yaml_expr = Exp.function_ ~loc to_yaml_cases in
             [
               pstr_value ~loc rec_flag
                 [ Vb.mk (ppat_var ~loc { loc; txt = to_yaml }) to_yaml_expr ];
             ]
         | { ptype_kind = Ptype_record fields; ptype_loc; ptype_name; _ } ->
             let to_yaml = mangle_name_label suf_to ptype_name.txt in
             [
               pstr_value ~loc rec_flag
                 [
                   Vb.mk
                     [%pat? [%p ppat_var ~loc { loc; txt = to_yaml }]]
                     [%expr
                       [%e
                         Helpers.poly_fun ~loc:ptype_loc typ_decl
                           (Value.record_to_expr
                              ~typ:(core_type_of_type_declaration typ_decl)
                              ~loc:ptype_loc fields)]];
                 ];
             ]
         | _ ->
             Location.raise_errorf ~loc "Cannot derive anything for this type")
       type_decls)

let generate_intf_to_yaml ~ctxt (_rec_flag, type_decls) :
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
                   txt =
                     mangle_name_label Helpers.suf_to typ_decl.ptype_name.txt;
                 }
                 (Value.type_decl_to_type typ_decl));
          ]
      | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
    type_decls
  |> List.concat

let generate_intf_of_yaml ~ctxt (_rec_flag, type_decls) :
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
                   txt =
                     mangle_name_label Helpers.suf_of typ_decl.ptype_name.txt;
                 }
                 (Value.type_decl_of_type typ_decl));
          ]
      | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
    type_decls
  |> List.concat

let impl_generator_to impl =
  Deriving.Generator.V2.make_noarg
    ~attributes:
      [
        Attribute.T Attrs.default; Attribute.T Attrs.name; Attribute.T Attrs.key;
      ]
    impl

let impl_generator_of impl =
  Deriving.Generator.V2.make
    ~attributes:
      [
        Attribute.T Attrs.default; Attribute.T Attrs.name; Attribute.T Attrs.key;
      ]
    Deriving.Args.(empty +> flag "skip_unknown")
    impl

let intf_generator intf = Deriving.Generator.V2.make_noarg intf

let deriver =
  let of_yaml =
    Deriving.add "of_yaml"
      ~str_type_decl:(impl_generator_of generate_impl_of_yaml)
      ~sig_type_decl:(intf_generator generate_intf_of_yaml)
  in
  let to_yaml =
    Deriving.add "to_yaml"
      ~str_type_decl:(impl_generator_to generate_impl_to_yaml)
      ~sig_type_decl:(intf_generator generate_intf_to_yaml)
  in
  Deriving.add_alias "yaml" [ of_yaml; to_yaml ]
