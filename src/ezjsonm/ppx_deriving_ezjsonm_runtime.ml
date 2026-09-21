type t = Ezjsonm.value

let int i = `Float (float_of_int i)
let float f = `Float f
let string s = `String s
let bool b = `Bool b
let list l = `A l
let obj b = `O b
let null = `Null
