let () =
  Alcotest.run "ocaml-data-transform"
    (Test_schema.suite
     @ Test_protobuf.suite
     @ Test_json_path.suite
     @ Test_transform_utils.suite)
