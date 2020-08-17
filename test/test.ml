let yaml = Alcotest.testable Yaml.pp Yaml.equal

type str = string [@@deriving yaml]

type flo = float [@@deriving yaml]

type integer = int [@@deriving yaml]

type boolean = bool [@@deriving yaml]

let test_primitives () =
  let correct_str = `String "hello world" in
  let test_str = str_to_yaml "hello world" in
  let correct_float = `Float 1.234 in
  let test_float = flo_to_yaml 1.234 in
  let correct_float_int = `Float 1. in
  let test_float_int = integer_to_yaml 1 in
  let correct_bool = `Bool true in
  let test_bool = boolean_to_yaml true in
  Alcotest.check yaml "same string" correct_str test_str;
  Alcotest.check yaml "same float" correct_float test_float;
  Alcotest.check yaml "same float from int" correct_float_int test_float_int;
  Alcotest.check yaml "same string" correct_bool test_bool

type person = { name : string; age : int } [@@deriving yaml]

type users = person list [@@deriving yaml]

let test_record_list () =
  let correct =
    `A
      [
        `O [ ("name", `String "Alice"); ("age", `Float 20.) ];
        `O [ ("name", `String "Bob"); ("age", `Float 21.) ];
      ]
  in
  let test =
    users_to_yaml [ { name = "Alice"; age = 20 }; { name = "Bob"; age = 21 } ]
  in
  Alcotest.check yaml "same object" correct test

type tup = int * string * float [@@deriving yaml]

let test_tuple () =
  let correct = `A [ `Float 1.; `String "OCaml"; `Float 3.14 ] in
  let test = tup_to_yaml (1, "OCaml", 3.14) in
  Alcotest.check yaml "same tuple (list)" correct test

type 'a pol = { txt : 'a } [@@deriving yaml]

let test_simple_poly () =
  let correct_str = `O [ ("txt", `String "arg0") ] in
  let test_str = pol_to_yaml (fun x -> `String x) { txt = "arg0" } in
  Alcotest.check yaml "same polymorhpic record" correct_str test_str

let tests : unit Alcotest.test_case list =
  [
    ("test_primitives", `Quick, test_primitives);
    ("test_record_list", `Quick, test_record_list);
    ("test_tuple", `Quick, test_tuple);
    ("test_simple_poly", `Quick, test_simple_poly);
  ]

let () = Alcotest.run "PPX Deriving Yaml" [ ("ppx", tests) ]
