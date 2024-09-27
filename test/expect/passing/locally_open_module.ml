module M = struct
  type t = A | B | C [@@deriving yaml]
end

type locally_open_m = Lom of M.(t) [@@deriving yaml]


let () =
  let v = Lom A in
  locally_open_m_to_yaml v
  |> Yaml.to_string_exn |> print_endline
