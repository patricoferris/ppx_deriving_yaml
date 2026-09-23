type var = Hello | World of string [@@deriving yamlx]
type poly_var = [ `Hello | `World of string ] [@@deriving yamlx]
type a = { x : [ `Simple | `Example ] } [@@deriving yamlx]
type t = { name : string; age : int option } [@@deriving yamlx]
type u = { name : string [@default "Una"] } [@@deriving to_yamlx]
type v = { age : int [@key "AGE"] [@default 10] } [@@deriving of_yamlx]

type w = {
  age : int;
      [@to_yamlx fun i -> YAMLx.Int (YAMLx.zero_loc, Int64.of_int (i - 10))]
}
[@@deriving yamlx]

type x = {
  age : int;
      [@of_yamlx
        function
        | YAMLx.Int (_, f) -> Ok (Int64.to_int Int64.(add f 10L))
        | _ -> Error (`Msg "Expected a Yaml `Float")]
}
[@@deriving yamlx]

type arr = int array [@@deriving yamlx]
