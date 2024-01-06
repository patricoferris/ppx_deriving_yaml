# ppx_deriving_yaml

This ppx is based on [ppx_yojson](https://github.com/NathanReb/ppx_yojson) and [ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) because of the many similarities between JSON and yaml. In particular many of the way the OCaml values are encoded to yaml types are the same as those implemented by the Yojson ppx.

- [Basic Usage](#basic-usage)
- [Attributes](#attributes)
  - [Key and Name](#key-and-name)
  - [Default Values](#default-values)
  - [Custom encoding and decoding](#custom-encoding-and-decoding)
- [Partially Decoding](#partially-decoding)
- [Implementation Details](#implementation-details)

## Basic Usage

For converting OCaml values to yaml values `ppx_deriving_yaml` will do the conventional dropping of the type name if it is `t`. Otherwise the type name is the prefix to the `to_yaml` function. 

`to_yaml` produces a [`Yaml.value`](https://github.com/avsm/ocaml-yaml/blob/master/lib/types.ml#L44) which is compatible with the [`Ezjsonm.value`](https://github.com/mirage/ezjsonm/blob/master/lib/ezjsonm.ml#L18) type. 

`of_yaml` produces OCaml types wrapped in a `result` -- this is how ocaml-yaml also handles errors i.e. not using exceptions. Based on your type this should let you move between yaml and OCaml values.

```ocaml
# #require "ppx_deriving_yaml";;
```

Here is a small example.

```ocaml
type person = { name : string; age : int } [@@deriving yaml]
type users = person list [@@deriving yaml]
```

This will produce four functions, a `_to_yaml` and `_of_yaml` for both a person and
the users. For example:

```ocaml
# person_to_yaml;;
- : person ->
    [> `O of (string * [> `Float of float | `String of string ]) list ]
= <fun>
# users_of_yaml;;
- : [> `A of [> `O of (string * Yaml.value) list ] list ] ->
    (person list, [> `Msg of string ]) result
= <fun>
```

If you make polymorphic types, then you will have to supply the function to convert the unknown to a yaml value. For example: 

```ocaml
type 'a note = { txt : 'a } [@@deriving yaml]
```

produces the following function. 

```ocaml
# note_to_yaml;;
- : ('a -> Yaml.value) -> 'a note -> [> `O of (string * Yaml.value) list ] =
<fun>
```

Finally, if you only need the encoder (`to_yaml`) or the decoder (`of_yaml`) then there are single versions of the deriver for those.

```ocaml
# type x = { age : int }[@@deriving to_yaml];;
type x = { age : int; }
val x_to_yaml : x -> [> `O of (string * [> `Float of float ]) list ] = <fun>
```

## Attributes

### Key and Name 

Record field names cannot begin with a capital letter and variant constructors must start with one. This limits what the generated yaml can look like. To override the yaml names you can use the `[@key <string>]` and `[@name <string>]` attributes for records and variants respectively. 

For example: 

```ocaml
type t = {
  camel_name : string [@key "camel-name"]
}[@@deriving to_yaml]
```

Will produce Yaml of the form 

```ocaml
# Yaml.to_string_exn (to_yaml { camel_name = "Alice" });;
- : string = "camel-name: Alice\n"
```

### Default Values

You can also specify default values for fields.

```ocaml
type t = {
  name : string;
  age : int [@default 42]
}[@@deriving yaml]
```

These will be used in the absence of any fields when decoding yaml values into OCaml ones.

```ocaml
# Yaml.of_string_exn "name: Alice" |> of_yaml;;
- : (t, [> `Msg of string ]) result = Ok {name = "Alice"; age = 42}
```

### Custom encoding and decoding

Sometimes you might want to specify your own encoding and decoding logic on field
by field basis. To do so, you can use the `of_yaml` and `to_yaml` attributes.

```ocaml
type t = {
  name : string [@to_yaml fun i -> `String ("custom-" ^ i)]
}[@@deriving yaml]
```

The `to_yaml` function will use the custom encoder now instead.

```ocaml
# Yaml.to_string_exn (to_yaml { name = "alice" });;
- : string = "name: custom-alice\n"
```

## Partially Decoding

There is a `~skip_unknown` flag for telling the deriver to simply ignore any fields which are missing. This is particularly useful when you only wish to partially decode a yaml value.

Consider the following yaml:

```ocaml
let yaml = "name: Bob\nage: 42\nmisc: We don't need this!"
```

If we try to do the normal decoding of this but only partially extract the fields, it will throw an error.

```ocaml
type t = {
  name : string;
  age : int;
}[@@deriving yaml]
```

Note that the error is often rather confusing. There is room for improvement (PRs welcome!).

```ocaml
# Yaml.of_string_exn yaml |> of_yaml;;
- : (t, [> `Msg of string ]) result =
Error (`Msg "miscWe don't need this!\n")
```

Instead we tell the deriver to ignore unknown fields.

```ocaml
type t = {
  name : string;
  age : int;
}[@@deriving yaml ~skip_unknown]
```

```ocaml
# Yaml.of_string_exn yaml |> of_yaml;;
- : (t, [> `Msg of string ]) result = Ok {name = "Bob"; age = 42}
```

## Implementation Details 

One important thing is that `'a option` values within records will return `None` if the Yaml you are trying to convert does not exist.

|            OCaml Type            |            Yaml Type            |
|:--------------------------------:|:-------------------------------:|
|               `int`              |           `` `Float ``          |
|              `float`             |           `` `Float ``          |
|             `string`             |          `` `String ``          |
|              `bool`              |           `` `Bool ``           |
|              `None`              |           `` `Null ``           |
|              `list`              |            `` `A []``           |
|              `array`             |            `` `A []``           |
| `record` e.g `{ name : string }` |  `` `O [("name", `String s)] `` |
|  `A of int` or `` [`A of int]``  | `` `O [("A", `A [`Float f])] `` |

