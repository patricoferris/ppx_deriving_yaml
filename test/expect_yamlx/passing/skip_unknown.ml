type t = { name : string; age : int } [@@deriving of_yamlx ~skip_unknown]
