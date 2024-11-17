type t = int -> int [@@deriving yaml]
type u = int -> int [@@deriving yaml]
type v = int [@@deriving yaml]
type w = .. [@@deriving yaml]
type x [@@deriving yaml]

let _ = v_of_yaml
