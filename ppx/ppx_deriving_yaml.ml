open Ppxlib
open Ast_helper
open Ast_builder.Default
open Ppx_deriving_yaml_lib

let suf_to = Helpers.suf_to

let suf_of = Helpers.suf_of

let mangle_name_label suff label =
  if label = "t" then suff else label ^ "_" ^ suff

let generate_impl ~ctxt (_rec_flag, type_decls) =
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
                 let ocamliser =
                   Helpers.poly_fun ~loc:typ_decl.ptype_loc typ_decl
                     (Value.of_yaml_type_to_expr None t)
                 in
                 let to_yaml = mangle_name_label suf_to ptype_name.txt in
                 let of_yaml = mangle_name_label suf_of ptype_name.txt in
                 [
                   pstr_value ~loc Nonrecursive
                     [ Vb.mk (ppat_var ~loc { loc; txt = to_yaml }) yamliser ];
                   pstr_value ~loc Nonrecursive
                     [ Vb.mk (ppat_var ~loc { loc; txt = of_yaml }) ocamliser ];
                 ]
             | None ->
                 Location.raise_errorf ~loc
                   "Cannot derive anything for this type")
         | { ptype_kind = Ptype_record fields; ptype_loc; ptype_name; _ } ->
             let to_yaml = mangle_name_label suf_to ptype_name.txt in
             let of_yaml = mangle_name_label suf_of ptype_name.txt in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk
                     (ppat_var ~loc { loc; txt = to_yaml })
                     (Helpers.poly_fun ~loc:ptype_loc typ_decl
                        (Value.record_to_expr
                           ~typ:(core_type_of_type_declaration typ_decl)
                           ~loc:ptype_loc fields));
                 ];
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk
                     (ppat_var ~loc { loc; txt = of_yaml })
                     (Helpers.poly_fun ~loc:ptype_loc typ_decl
                        (Value.of_yaml_record_to_expr ~loc:ptype_loc fields));
                 ];
             ]
         | _ ->
             Location.raise_errorf ~loc "Cannot derive anything for this type")
       type_decls)

let generate_intf ~ctxt (_rec_flag, type_decls) : Ppxlib.Ast.signature_item list
    =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun typ_decl ->
      match typ_decl with
      | { ptype_kind = Ptype_abstract | Ptype_record _; _ } ->
          psig_value ~loc
            (Val.mk
               {
                 loc = typ_decl.ptype_name.loc;
                 txt = mangle_name_label Helpers.suf_to typ_decl.ptype_name.txt;
               }
               (Value.type_decl_to_type typ_decl))
      | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
    type_decls

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let to_yaml =
  Deriving.add "yaml" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
