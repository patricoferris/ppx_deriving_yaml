type t = Ezjsonm.value
type integer = float

let int i = `Float (float_of_int i)
let float f = `Float f
let string s = `String s
let bool b = `Bool b
let list l = `A l
let obj b = `O b
let null = `Null
let of_integer = Float.to_int
