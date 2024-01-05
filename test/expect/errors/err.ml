type t = int -> int [@@deriving yaml]
type u = int -> int [@@deriving yaml]
type v = int [@@deriving yaml]

let _ = v_of_yaml