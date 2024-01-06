type hobbies = { sport : string; music : string } [@@deriving yaml]

type t = { name : string; age : int; hobbies : hobbies [@inline] }
[@@deriving yaml]
