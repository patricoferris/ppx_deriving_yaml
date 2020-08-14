type a = int list [@@deriving yaml]

let () =
  let x : Person.t = "Patrick" in
  ignore (Person.to_yaml x)
