include
  Ppx_deriving_ezjsonm_types.Runtime
    with type t = Ezjsonm.value
     and type integer = float
