include
  Ppx_deriving_yaml_types.Runtime
    with type t = Yaml.value
     and type integer = float
