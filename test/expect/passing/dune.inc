; -------- Test: `simple.ml` --------

; The PPX-dependent executable under test
(executable
 (name simple)
 (modules simple)
 (preprocess (pps ppx_deriving_yaml))
 (libraries yaml))

; Run the PPX on the `.ml` file
(rule
 (targets simple.actual)
 (deps
  (:pp pp.exe)
  (:input simple.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_yaml)
 (action
  (diff simple.expected simple.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_deriving_yaml)
 (action
  (run ./simple.exe)))

; -------- Test: `skip_unknown.ml` --------

; The PPX-dependent executable under test
(executable
 (name skip_unknown)
 (modules skip_unknown)
 (preprocess (pps ppx_deriving_yaml))
 (libraries yaml))

; Run the PPX on the `.ml` file
(rule
 (targets skip_unknown.actual)
 (deps
  (:pp pp.exe)
  (:input skip_unknown.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_yaml)
 (action
  (diff skip_unknown.expected skip_unknown.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_deriving_yaml)
 (action
  (run ./skip_unknown.exe)))


