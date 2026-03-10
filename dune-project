(lang dune 3.16)

(name ocaml_data_transform)

(generate_opam_files true)

(package
 (name ocaml_data_transform)
 (synopsis "OCaml port of JSONata data transformation pipeline")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.16))
  ocaml-protoc
  pbrt
  pbrt_yojson
  (yojson (>= 2.0))
  (re (>= 1.11))
  (alcotest (and (>= 1.7) :with-test))))
