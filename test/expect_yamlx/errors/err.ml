type t = int -> int [@@deriving yamlx]
type u = int -> int [@@deriving yamlx]
type v = int [@@deriving yamlx]
type w = .. [@@deriving yamlx]
type x [@@deriving yamlx]

let _ = v_of_yamlx
