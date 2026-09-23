include
  Ppx_deriving_yaml_types.Runtime
    with type t = YAMLx.value
     and type integer = int64
