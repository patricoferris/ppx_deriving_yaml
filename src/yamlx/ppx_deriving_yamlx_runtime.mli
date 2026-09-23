include
  Ppx_deriving_yamlx_types.Runtime
    with type t = YAMLx.value
     and type integer = int64
