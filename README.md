## Ppx_deriving_yaml -- OCaml types to YAML types 

This ppx is based on [ppx_yojson](https://github.com/NathanReb/ppx_yojson) because of the many similarities between json and yaml.

This is a small ppx deriver that lets you convert your OCaml types to [yaml](https://github.com/avsm/ocaml-yaml) ones. This means you can describe yaml structures in OCaml and easily convert them to yaml.


### Checklist 

- [x] Simples types (`int, list, records...`) to `Yaml.value` types
- [x] `Yaml.value` interface types 
- [] `Yaml.value` types to OCaml types i.e. `of_yaml` 
- [] Simples types (`int, list, records...`) to `Yaml.yaml` types
- [] More complex types (parametrics polymorphic ones) to any of the Yaml types 