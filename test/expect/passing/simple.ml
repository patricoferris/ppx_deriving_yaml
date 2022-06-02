type t = { name : string; age : int option } [@@deriving yaml]
type u = { name : string [@default "Una"] } [@@deriving to_yaml]
type v = { age : int [@key "AGE"] [@default 10] } [@@deriving of_yaml]
