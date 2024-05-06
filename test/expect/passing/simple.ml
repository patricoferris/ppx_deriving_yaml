type var = Hello | World of string [@@deriving yaml]
type poly_var = [ `Hello | `World of string ] [@@deriving yaml]
type a = { x : [ `Simple | `Example ] } [@@deriving yaml]
type t = { name : string; age : int option } [@@deriving yaml]
type u = { name : string [@default "Una"] } [@@deriving to_yaml]
type v = { age : int [@key "AGE"] [@default 10] } [@@deriving of_yaml]

type w = { age : int [@to_yaml fun i -> `Float (float_of_int (i - 10))] }
[@@deriving yaml]

type x = {
  age : int;
      [@of_yaml
        function
        | `Float f -> Ok (int_of_float (f +. 10.))
        | _ -> Error (`Msg "Expected a Yaml `Float")]
}
[@@deriving yaml]
