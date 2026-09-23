type t = YAMLx.value
type integer = int64

let int i = YAMLx.Int (YAMLx.zero_loc, Int64.of_int i)
let float f = YAMLx.Float (YAMLx.zero_loc, f)
let string s = YAMLx.String (YAMLx.zero_loc, s)
let bool b = YAMLx.Bool (YAMLx.zero_loc, b)
let list l = YAMLx.Seq (YAMLx.zero_loc, l)

let obj b =
  let assoc = List.map (fun (s, v) -> (YAMLx.zero_loc, string s, v)) b in
  YAMLx.Map (YAMLx.zero_loc, assoc)

let null = YAMLx.Null YAMLx.zero_loc
let of_integer i64 = Int64.to_int i64
