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
 (synopsis "Yaml PPX Deriver")
 (description "Deriving conversion functions to and from yaml for your OCaml types.")
 (depends
  yaml
  (alcotest :with-test)
  (mdx (and :with-test (>= 2.0.0)))
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.25.0))
  ))

(package
 (name ppx_deriving_ezjsonm)
 (synopsis "Yaml PPX Deriver")
 (description "Deriving conversion functions to and from JSON for your OCaml types.")
 (depends
  ezjsonm
  (alcotest :with-test)
  (mdx (and :with-test (>= 2.0.0)))
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.25.0))
  ))

(using mdx 0.3)
