(lang dune 3.14)

(name ppx_deriving_yaml)

(generate_opam_files true)

(source
 (github patricoferris/ppx_deriving_yaml))

(license ISC)

(authors "Patrick Ferris")

(maintainers "patrick@sirref.org")

(package
 (name ppx_deriving_yaml)
 (synopsis "Yaml PPX")
 (description "Deriving conversion functions to and from yaml for your OCaml types.")
 (tags ("ppx" "deriver" "yaml"))
 (depends
  yaml
  (alcotest :with-test)
  (mdx (and :with-test (>= 2.0.0)))
  (ppx_deriving :with-test) ; dune still puts this in META file ...
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.25.0))
  ))

(package
 (name ppx_deriving_ezjsonm)
 (synopsis "Ezjsonm PPX")
 (description "Deriving conversion functions to and from JSON for your OCaml types.")
 (tags ("ppx" "deriver" "json"))
 (depends
  ezjsonm
  (alcotest :with-test)
  (mdx (and :with-test (>= 2.0.0)))
  (ppx_deriving :with-test) ; dune still puts this in META file ...
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.25.0))
  ))

(using mdx 0.3)
