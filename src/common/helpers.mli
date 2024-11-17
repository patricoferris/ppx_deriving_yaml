open Ppxlib

val mkloc : 'a -> 'a Ppxlib.loc
val arg : int -> string
val mangle_suf : ?fixpoint:string -> string -> Longident.t -> Longident.t
val poly_fun : loc:location -> type_declaration -> expression -> expression
val ptuple : loc:location -> pattern list -> pattern
val etuple : loc:location -> expression list -> expression
val map_bind : loc:location -> expression
