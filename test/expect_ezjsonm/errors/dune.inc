(env
   (_
    (env-vars
     (OCAML_ERROR_STYLE "short")
     (OCAML_COLOR "never"))))

  ; -------- Test: `err.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets err.actual)
 
 (deps
  (:pp pp.exe)
  (:input err.ml))
 (action
  (with-stdout-to
   %{targets}
   (bash "./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 
 (package ppx_deriving_ezjsonm)
 (action
  (diff err.expected err.actual)))


