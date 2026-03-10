(lang dune 3.16)

(name ocaml_data_transform)

(generate_opam_files true)

(package
 (name ocaml_data_transform)
 (synopsis "OCaml port of JSONata data transformation pipeline")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.16))
  (ocaml-protoc (>= 3.1))
  (ocaml-protoc-yojson (>= 3.1))
  (yojson (>= 2.0))
  (re (>= 1.11))
  (alcotest (and (>= 1.7) :with-test))))
