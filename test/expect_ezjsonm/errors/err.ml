type t = int -> int [@@deriving ezjsonm]
type u = int -> int [@@deriving ezjsonm]
type v = int [@@deriving ezjsonm]
type w = .. [@@deriving ezjsonm]
type x [@@deriving ezjsonm]

let _ = v_of_ezjsonm
