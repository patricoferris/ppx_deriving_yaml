(lang dune 2.6)

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
  (alcotest :with-test)
  (bos :with-test)
  (mdx :with-test)
  (ezjsonm :with-test)
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.14.0))
  ppx_deriving ; needed for https://github.com/ocaml/dune/issues/1327
  yaml))

(using mdx 0.1)
