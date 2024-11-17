# Derivers for Yaml and JSON

This repository contains the source code for:

 - [ppx_deriving_yaml] a ppx deriver for generating conversion functions for Yaml.
 - [ppx_deriving_ezjsonm] a ppx deriver for generating conversation functions for JSON (using the [Ezjsonm][] library).

## Installation

You may need to update your opam-repository.

```sh
opam update
opam install ppx_deriving_yaml     # For the Yaml deriver
opam install ppx_deriving_ezjsonm  # For the Ezjsonm deriver
```

## Documentation

The release documentation should be available on the OCaml.org website at https://ocaml.org/p/ppx_deriving_yaml
and https://ocaml.org/p/ppx_deriving_ezjsonm.

[The latest documentation is available here](https://patricoferris.github.io/ppx_deriving_yaml).

The documentation contains sample programs.


[Ezjsonm]: https://ocaml.org/p/ezjsonm
