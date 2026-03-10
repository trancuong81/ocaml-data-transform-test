let test_version () =
  Alcotest.(check string) "version" "0.1.0" Ocaml_data_transform.Placeholder.version

let () =
  Alcotest.run "ocaml-data-transform" [
    ("smoke", [
      Alcotest.test_case "version" `Quick test_version;
    ]);
  ]
