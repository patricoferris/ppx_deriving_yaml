## Ppx_deriving_yaml -- OCaml types to YAML types 

This ppx is based on [ppx_yojson](https://github.com/NathanReb/ppx_yojson) and [ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) because of the many similarities between json and yaml. In particular many of the semantics of OCaml types <-> Yaml types are the same as those implemented by the Yojson ppx.

This is a small ppx deriver that lets you convert your OCaml types to [yaml](https://github.com/avsm/ocaml-yaml) ones. This means you can describe yaml structures in OCaml and easily convert them to yaml.

### Usage

**The usage docs are linked to what has been implemented from the checklist**

For converting OCaml types to Yaml types `ppx_deriving_yaml` will do the conventional dropping of the type name if it is `t`. Otherwise the type name is the prefix to the `to_yaml` function. 

`to_yaml` produces a [`Yaml.value`](https://github.com/avsm/ocaml-yaml/blob/master/lib/types.ml#L44) which is compatible with the [`Ezjsonm.value`](https://github.com/mirage/ezjsonm/blob/master/lib/ezjsonm.ml#L18) type. 

`of_yaml` produces OCaml types wrapped in a `Rresult.R.t` -- this is how ocaml-yaml also handles errors i.e. not using exceptions. Based on your type this should let you move between yaml and OCaml types.

Here is a small example. 

```ocaml
type person = { name : string; age : int } [@@deriving yaml]
type users = person list [@@deriving yaml]
```

This will produce four functions. 

```ocaml
val person_to_yaml : person -> Yaml.value 
val person_of_yaml : Yaml.value -> (person, [> `Msg of string ]) result 
val users_to_yaml : users -> Yaml.value 
val users_of_yaml : Yaml.value -> (users, [> `Msg of string ]) result 
```

If you make polymorphic types, then you will have to supply the function to convert the unknown to a yaml value. For example: 

```ocaml
type 'a note = { txt : 'a } [@@deriving yaml]
```

produces the following function. 

```ocaml
val note_to_yaml : ('a -> Yaml.value) -> 'a note -> [> `O of (string * Yaml.value) list ]
```

### Implementation Details 

One important thing is that `'a option` values within records will return `None` if the Yaml you are trying to convert does not exist.

### Checklist 

- [x] Simples types (`int, list, records...`) to `Yaml.value` types
- [x] `Yaml.value` interface types 
- [x] `Yaml.value` types to OCaml types i.e. `of_yaml` 
- [x] More complex types (parametric polymorphic ones) to any of the Yaml types 
- [ ] Design and implement how variants should be handled
- [ ] Better interface support i.e. `.mli` files 
- [ ] Simple types (`int, list, records...`) to `Yaml.yaml` types
