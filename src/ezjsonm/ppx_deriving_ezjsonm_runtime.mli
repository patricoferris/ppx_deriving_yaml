include
  Ppx_deriving_yaml_types.Runtime
    with type t = Ezjsonm.value
     and type integer = float
