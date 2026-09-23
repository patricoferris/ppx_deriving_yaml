# Derivers for Yaml and JSON

This repository contains the source code for:

 - `ppx_deriving_yaml` a ppx deriver for [Yaml][].
 - `ppx_deriving_ezjsonm` a ppx deriver for JSON using the [Ezjsonm][] library.
 - `ppx_deriving_yamlx` a ppx deriver for [YAMLx][].

## Installation

You may need to update your opam-repository.

```sh
opam update
opam install ppx_deriving_yaml     # For the Yaml deriver
opam install ppx_deriving_ezjsonm  # For the Ezjsonm deriver
opam install ppx_deriving_yamlx    # For the YAMLx deriver
```

## Documentation

[The latest documentation is available here](https://patricoferris.github.io/ppx_deriving_yaml).

The documentation contains sample programs.

[Yaml]: https://ocaml.org/p/yaml
[Ezjsonm]: https://ocaml.org/p/ezjsonm
[YAMLx]: https://ocaml.org/p/yamlx
