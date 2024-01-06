(lang dune 2.7)

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
  ppx_deriving ; needed for https://github.com/ocaml/dune/issues/1327
  (alcotest :with-test)
  (mdx (and :with-test (>= 2.0.0)))
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.25.0))
  ))

(using mdx 0.1)
