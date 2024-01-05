## v0.2.2 (05/01/2024) Cambridge

 - Embed errors in the AST (#51, @patricoferris and special thanks to @panglesd
   for the detailed issue in #48)

## v0.2.1 (04/12/2022) Cambridge

 - Support types with recursive definitions (#46, @patricoferris)
 - Fix `skip_unknown` flag when unknown fields are not last in the record (#43, @code-ghalib)

## v0.2.0 (14/10/2022)

- Add custom `to_yaml` and `of_yaml` attributes (#38, @patricoferris)
- Add `skip_unknown` flag to allow partially decoding yaml (#40, @code-ghalib)
- Hide record fields with default values in to_yaml output (#37, @maurobringolf, reviewed by @sim642 and @patricoferris)
- Expose `to_yaml` and `of_yaml` derivers with `yaml` being an alias (#36, @patricoferris)
- Improved error messages (#32, @prosper74, reviewed by @patricoferris)
- Add a default attribute for providing placeholder values (#31, @prosper74, reviewed by @ayc9, @pitag-ha and @patricoferris)

## v0.1.1 (28/02/2022)

- Remove rresult dependency (#27, @patricoferris)

## v0.1.0

- Initial public release
