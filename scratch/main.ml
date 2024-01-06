type hobbies = {
  sport: string ;
  music: string }[@@deriving yaml]
include
  struct
    let hobbies_to_yaml (x : hobbies) =
      `O
        (List.filter_map (fun x -> x)
           [Some ("sport", (((fun (x : string) -> `String x)) x.sport));
           Some ("music", (((fun (x : string) -> `String x)) x.music))])
    let hobbies_of_yaml =
      let (>>=) v f = match v with | Ok v -> f v | Error _ as e -> e in
      function
      | `O xs ->
          let rec loop xs ((arg0, arg1) as _state) =
            match xs with
            | ("sport", x)::xs ->
                loop xs
                  (((function
                     | `String x -> Ok x
                     | _ ->
                         Error
                           (`Msg
                              "Was expecting 'string' but got a different type"))
                      x), arg1)
            | ("music", x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `String x -> Ok x
                      | _ ->
                          Error
                            (`Msg
                               "Was expecting 'string' but got a different type"))
                       x))
            | [] ->
                arg1 >>=
                  ((fun arg1 ->
                      arg0 >>=
                        (fun arg0 -> Ok { sport = arg0; music = arg1 })))
            | (x, y)::_ -> Error (`Msg (x ^ (Yaml.to_string_exn y))) in
          loop xs
            ((Error (`Msg "Didn't find the function for key: sport")),
              (Error (`Msg "Didn't find the function for key: music")))
      | _ ->
          Error (`Msg "Failed building a key-value object expecting a list")
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t = {
  name: string ;
  age: int ;
  hobbies: hobbies [@inline ]}[@@deriving yaml]
include
  struct
    let to_yaml (x : t) =
      `O
        (List.filter_map (fun x -> x)
           [Some ("name", (((fun (x : string) -> `String x)) x.name));
           Some ("age", (((fun (x : int) -> `Float (float_of_int x))) x.age));
           Some ("hobbies", (((fun x -> hobbies_to_yaml x)) x.hobbies))])
    let of_yaml =
      let (>>=) v f = match v with | Ok v -> f v | Error _ as e -> e in
      function
      | `O xs ->
          let rec loop xs ((arg0, arg1, arg2) as _state) =
            match xs with
            | ("name", x)::xs ->
                loop xs
                  (((function
                     | `String x -> Ok x
                     | _ ->
                         Error
                           (`Msg
                              "Was expecting 'string' but got a different type"))
                      x), arg1, arg2, arg3)
            | ("age", x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `Float x -> Ok (int_of_float x)
                      | _ ->
                          Error
                            (`Msg
                               "Was expecting 'int' but got a different type"))
                       x), arg2, arg3)
            | ("sport", x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `String x -> Ok x
                      | _ ->
                          Error
                            (`Msg
                               "Was expecting 'string' but got a different type"))
                       x), arg3)
            | ("music", x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `String x -> Ok x
                      | _ ->
                          Error
                            (`Msg
                               "Was expecting 'string' but got a different type"))
                       x))
            | [] ->
                arg2 >>=
                  ((fun arg2 ->
                      arg1 >>=
                        (fun arg1 ->
                           arg0 >>=
                             (fun arg0 ->
                                Ok
                                  { name = arg0; age = arg1; hobbies = arg2 }))))
            | (x, y)::_ -> Error (`Msg (x ^ (Yaml.to_string_exn y))) in
          loop xs
            ((Error (`Msg "Didn't find the function for key: name")),
              (Error (`Msg "Didn't find the function for key: age")),
              (Error (`Msg "Didn't find the function for key: hobbies")))
      | _ ->
          Error (`Msg "Failed building a key-value object expecting a list")
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
