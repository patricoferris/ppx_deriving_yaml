(lang dune 2.6)

(name ppx_deriving_yaml)

(generate_opam_files true)

(source
 (github patricoferris/ppx_deriving_yaml))

(license ISC)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(package
 (name ppx_deriving_yaml)
 (synopsis "Yaml PPX Derivier")
 (description "Build Yaml types from OCaml types")
 (depends
  (alcotest :with-test)
  (bos :with-test)
  (mdx :with-test)
  (ezjsonm :with-test)
  (ocaml
   (>= 4.08.1))
  (ppxlib
   (>= 0.14.0))
  (ppx_deriving
   (>= 4.5))
  yaml
  rresult))

(using mdx 0.1)
