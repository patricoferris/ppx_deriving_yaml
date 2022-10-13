type t = { name : string; age : int } [@@deriving of_yaml ~skip_unknown]
