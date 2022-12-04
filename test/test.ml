let yaml = Alcotest.testable Yaml.pp Yaml.equal

type error = [ `Msg of string ]

let pp_error ppf (`Msg x) = Fmt.string ppf x
let error = Alcotest.testable pp_error ( = )

type str = string [@@deriving yaml]
type flo = float [@@deriving yaml]

let flo = Alcotest.testable Format.pp_print_float Stdlib.( = )

type integer = int [@@deriving yaml]
type boolean = bool [@@deriving yaml]

let test_primitives () =
  let correct_str = `String "hello world" in
  let test_str = str_to_yaml "hello world" in
  let correct_str_of = "hello world" in
  let test_str_of = str_of_yaml correct_str in
  let correct_float = `Float 1.234 in
  let test_float = flo_to_yaml 1.234 in
  let correct_float_of = 1.234 in
  let test_float_of = flo_of_yaml correct_float in
  let correct_float_int = `Float 1. in
  let test_float_int = integer_to_yaml 1 in
  let correct_float_int_of = 1 in
  let test_float_int_of = integer_of_yaml correct_float_int in
  let correct_bool = `Bool true in
  let test_bool = boolean_to_yaml true in
  let correct_bool_of = true in
  let test_bool_of = boolean_of_yaml correct_bool in
  Alcotest.check yaml "(to_yaml) same string" correct_str test_str;
  Alcotest.(check (result string error))
    "(of_yaml) same string" (Ok correct_str_of) test_str_of;
  Alcotest.check yaml "(to_yaml) same float" correct_float test_float;
  Alcotest.(check (result flo error))
    "(of_yaml) same float" (Ok correct_float_of) test_float_of;
  Alcotest.check yaml "(to_yaml) same float from int" correct_float_int
    test_float_int;
  Alcotest.(check (result int error))
    "(of_yaml) same int from int" (Ok correct_float_int_of) test_float_int_of;
  Alcotest.check yaml "(to_yaml) same bool" correct_bool test_bool;
  Alcotest.(check (result bool error))
    "(of_yaml) same bool" (Ok correct_bool_of) test_bool_of

type person = { name : string; [@default "Alice"] age : int } [@@deriving yaml]

let pp_person ppf x =
  Format.pp_print_string ppf x.name;
  Format.pp_print_int ppf x.age

let person = Alcotest.testable pp_person Stdlib.( = )

type users = { db : person list } [@@deriving yaml]

let users =
  Alcotest.testable
    (fun ppf users -> List.iter (pp_person ppf) users.db)
    Stdlib.( = )

let test_record_list () =
  let correct =
    `O
      [
        ( "db",
          `A
            [
              `O [ ("age", `Float 20.) ];
              `O [ ("name", `String "Bob"); ("age", `Float 21.) ];
            ] );
      ]
  in
  let with_default_correct =
    `O
      [
        ( "db",
          `A
            [
              `O [ ("age", `Float 20.) ];
              `O [ ("name", `String "Bob"); ("age", `Float 21.) ];
            ] );
      ]
  in
  let test =
    users_to_yaml
      { db = [ { name = "Alice"; age = 20 }; { name = "Bob"; age = 21 } ] }
  in
  let correct_of : (users, [> `Msg of string ]) result =
    Ok { db = [ { name = "Alice"; age = 20 }; { name = "Bob"; age = 21 } ] }
  in
  let test_of = users_of_yaml correct in
  let test_of_default = users_of_yaml with_default_correct in
  Alcotest.check yaml "(to_yaml) same object" correct test;
  Alcotest.(check (result users error))
    "(of_yaml) same object" correct_of test_of;
  Alcotest.(check (result users error))
    "(of_yaml) same object" correct_of test_of_default

type tup = int * string * float [@@deriving yaml]

let tup =
  Alcotest.testable
    (fun ppf (a, b, c) ->
      Format.pp_print_int ppf a;
      Format.pp_print_string ppf b;
      Format.pp_print_float ppf c)
    Stdlib.( = )

let test_tuple () =
  let correct = `A [ `Float 1.; `String "OCaml"; `Float 3.14 ] in
  let test = tup_to_yaml (1, "OCaml", 3.14) in
  let correct_of = Ok (1, "OCaml", 3.14) in
  let test_of = tup_of_yaml correct in
  Alcotest.check yaml "same tuple (list)" correct test;
  Alcotest.(check (result tup error)) "(of_yaml) same tuple" correct_of test_of

type 'a pol = { txt : 'a } [@@deriving yaml]

let str_pol =
  Alcotest.testable (fun ppf x -> Format.pp_print_string ppf x.txt) Stdlib.( = )

let test_simple_poly () =
  let correct_str = `O [ ("txt", `String "arg0") ] in
  let test_str = pol_to_yaml (fun x -> `String x) { txt = "arg0" } in
  let correct_str_of = { txt = "arg0" } in
  let test_str_of =
    pol_of_yaml (function `String s -> Ok s | _ -> failwith "") correct_str
  in
  Alcotest.check yaml "(to_yaml) same polymorhpic record" correct_str test_str;
  Alcotest.(check (result str_pol error))
    "(of_yaml) same polymorhpic record" (Ok correct_str_of) test_str_of

type str_opt = { name : string option } [@@deriving yaml]

let str_opt =
  Alcotest.testable
    (fun ppf x ->
      match x.name with
      | Some t -> Format.pp_print_string ppf t
      | None -> Format.pp_print_string ppf "none")
    Stdlib.( = )

let test_option () =
  let correct_opt_some = `O [ ("name", `String "Alice") ] in
  let test_opt_some = str_opt_to_yaml { name = Some "Alice" } in
  let correct_opt_none = `O [ ("name", `Null) ] in
  let test_opt_none = str_opt_to_yaml { name = None } in
  let correct_opt_some_of = Ok { name = Some "Alice" } in
  let test_opt_some_of = str_of_yaml correct_opt_some in
  let correct_opt_none_of = Ok { name = None } in
  let test_opt_none_of = str_of_yaml correct_opt_none in
  Alcotest.check yaml "same string option (some)" correct_opt_some test_opt_some;
  Alcotest.check yaml "same string option (none)" correct_opt_none test_opt_none;
  Alcotest.(check (result str_opt error))
    "(of_yaml) same string option (some)" correct_opt_some_of test_opt_some_of;
  Alcotest.(check (result str_opt error))
    "(of_yaml) same string option (none)" correct_opt_none_of test_opt_none_of

type var = Alpha | Beta of int | Gamma of string * int [@@deriving yaml]

let var =
  Alcotest.testable
    (fun ppf -> function
      | Alpha -> Fmt.string ppf "Alpha"
      | Beta i -> Fmt.pf ppf "Beta %i" i
      | Gamma (s, i) -> Fmt.pf ppf "Gamma (%s,%i)" s i)
    Stdlib.( = )

let test_var () =
  let correct_yaml_a = `O [ ("Alpha", `A []) ] in
  let test_yaml_a = var_to_yaml Alpha in
  let correct_yaml_b = `O [ ("Beta", `A [ `Float 3. ]) ] in
  let test_yaml_b = var_to_yaml (Beta 3) in
  let correct_yaml_c = `O [ ("Gamma", `A [ `String "hello"; `Float 3. ]) ] in
  let test_yaml_c = var_to_yaml (Gamma ("hello", 3)) in
  let correct_yaml_a_of = Ok Alpha in
  let test_yaml_a_of = var_of_yaml correct_yaml_a in
  let correct_yaml_b_of = Ok (Beta 3) in
  let test_yaml_b_of = var_of_yaml correct_yaml_b in
  let correct_yaml_c_of = Ok (Gamma ("hello", 3)) in
  let test_yaml_c_of = var_of_yaml correct_yaml_c in
  Alcotest.check yaml "same variant" correct_yaml_a test_yaml_a;
  Alcotest.check yaml "same variant" correct_yaml_b test_yaml_b;
  Alcotest.check yaml "same variant" correct_yaml_c test_yaml_c;
  Alcotest.(check (result var error))
    "(of_yaml) same variant" correct_yaml_a_of test_yaml_a_of;
  Alcotest.(check (result var error))
    "(of_yaml) same variant" correct_yaml_b_of test_yaml_b_of;
  Alcotest.(check (result var error))
    "(of_yaml) same variant" correct_yaml_c_of test_yaml_c_of

type poly_var = [ `Alpha | `Beta of int | `Gamma of string * int ]
[@@deriving yaml]

let poly_var : poly_var Alcotest.testable =
  Alcotest.testable
    (fun ppf -> function
      | `Alpha -> Fmt.pf ppf "Alpha"
      | `Beta i -> Fmt.pf ppf "Beta %i" i
      | `Gamma (s, i) -> Fmt.pf ppf "Gamma (%s, %i)" s i)
    Stdlib.( = )

let test_poly_variants () =
  let correct_yaml_a = `O [ ("Alpha", `A []) ] in
  let test_yaml_a = poly_var_to_yaml `Alpha in
  let correct_yaml_b = `O [ ("Beta", `A [ `Float 3. ]) ] in
  let test_yaml_b = poly_var_to_yaml (`Beta 3) in
  let correct_yaml_c = `O [ ("Gamma", `A [ `String "hello"; `Float 3. ]) ] in
  let test_yaml_c = poly_var_to_yaml (`Gamma ("hello", 3)) in
  let correct_yaml_a_of = Ok `Alpha in
  let test_yaml_a_of = poly_var_of_yaml correct_yaml_a in
  let correct_yaml_b_of = Ok (`Beta 3) in
  let test_yaml_b_of = poly_var_of_yaml correct_yaml_b in
  let correct_yaml_c_of = Ok (`Gamma ("hello", 3)) in
  let test_yaml_c_of = poly_var_of_yaml correct_yaml_c in
  Alcotest.check yaml "same polymorphic variant" correct_yaml_a test_yaml_a;
  Alcotest.check yaml "same polymorphic variant" correct_yaml_b test_yaml_b;
  Alcotest.check yaml "same polymorphic variant" correct_yaml_c test_yaml_c;
  Alcotest.(check (result poly_var error))
    "(of_yaml) same polymorphic variant" correct_yaml_a_of test_yaml_a_of;
  Alcotest.(check (result poly_var error))
    "(of_yaml) same polymorphic variant" correct_yaml_b_of test_yaml_b_of;
  Alcotest.(check (result poly_var error))
    "(of_yaml) same polymorphic variant" correct_yaml_c_of test_yaml_c_of

(** Attributes *)
type vattrib = Camel of int [@name "camel"] [@@deriving yaml]

type rattrib = { camel_name : string [@key "camel-name"] } [@@deriving yaml]

let vattrib =
  Alcotest.testable (fun ppf (Camel i) -> Fmt.int ppf i) Stdlib.( = )

let rattrib =
  Alcotest.testable
    (fun ppf v -> Fmt.pf ppf "{ camel-name: %s }" v.camel_name)
    Stdlib.( = )

let test_attrib () =
  let correct_yaml_v = `O [ ("camel", `A [ `Float 1. ]) ] in
  let test_yaml_v = vattrib_to_yaml (Camel 1) in
  let correct_yaml_r = `O [ ("camel-name", `String "lawrence") ] in
  let test_yaml_r = rattrib_to_yaml { camel_name = "lawrence" } in
  let correct_yaml_v_of = Ok (Camel 1) in
  let test_yaml_v_of = vattrib_of_yaml correct_yaml_v in
  let correct_yaml_r_of = Ok { camel_name = "lawrence" } in
  let test_yaml_r_of = rattrib_of_yaml correct_yaml_r in
  Alcotest.check yaml "same variant" correct_yaml_v test_yaml_v;
  Alcotest.check yaml "same record" correct_yaml_r test_yaml_r;
  Alcotest.(check (result vattrib error))
    "(of_yaml) same variant" correct_yaml_v_of test_yaml_v_of;
  Alcotest.(check (result rattrib error))
    "(of_yaml) same record" correct_yaml_r_of test_yaml_r_of

type unknown = { name : string; age : int } [@@deriving yaml ~skip_unknown]

let unknown = Alcotest.of_pp (fun ppf v -> Yaml.pp ppf (unknown_to_yaml v))

let test_unknown () =
  let yaml = "name: Bob\nmisc: We don't need this!\nage: 42" in
  let v = Yaml.of_string_exn yaml |> unknown_of_yaml in
  let expected = Ok { name = "Bob"; age = 42 } in
  Alcotest.(check (result unknown error)) "same unknown" expected v

let tests : unit Alcotest.test_case list =
  [
    ("test_primitives", `Quick, test_primitives);
    ("test_record_list", `Quick, test_record_list);
    ("test_tuple", `Quick, test_tuple);
    ("test_simple_poly", `Quick, test_simple_poly);
    ("test_var", `Quick, test_var);
    ("test_poly_variants", `Quick, test_poly_variants);
    ("test_attrib", `Quick, test_attrib);
    ("test_unknown", `Quick, test_unknown);
  ]

let () = Alcotest.run "ppx_deriving_yaml" [ ("ppx", tests) ]
