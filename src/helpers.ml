open Ppxlib
open Ast_helper

let arg n = "arg" ^ string_of_int n
let mkloc txt = { txt; loc = !Ast_helper.default_loc }
let suf_to = "to_yaml"
let suf_of = "of_yaml"

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
