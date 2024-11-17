type var = Hello | World of string [@@deriving ezjsonm]
type poly_var = [ `Hello | `World of string ] [@@deriving ezjsonm]
type a = { x : [ `Simple | `Example ] } [@@deriving ezjsonm]
type t = { name : string; age : int option } [@@deriving ezjsonm]
type u = { name : string [@default "Una"] } [@@deriving to_ezjsonm]
type v = { age : int [@key "AGE"] [@default 10] } [@@deriving of_ezjsonm]

type w = { age : int [@to_ezjsonm fun i -> `Float (float_of_int (i - 10))] }
[@@deriving ezjsonm]

type x = {
  age : int;
      [@of_ezjsonm
        function
        | `Float f -> Ok (int_of_float (f +. 10.))
        | _ -> Error (`Msg "Expected a Yaml `Float")]
}
[@@deriving ezjsonm]
