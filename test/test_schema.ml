let test_load_data_type_constants () =
  let constants = Ocaml_data_transform.Schema.load_data_type_constants () in
  (* Verify Ssn has correct regex *)
  let ssn = Ocaml_data_transform.Schema.find_type_constants constants "Ssn" in
  Alcotest.(check bool) "Ssn found" true (Option.is_some ssn);
  let ssn = Option.get ssn in
  Alcotest.(check bool) "Ssn has regex" true
    (Ocaml_data_transform.Schema.has_regex ssn);
  (* Verify total count *)
  let count = Ocaml_data_transform.Schema.type_count constants in
  Alcotest.(check int) "47 types" 47 count

let suite = [
  ("schema", [
    Alcotest.test_case "load data type constants" `Quick
      test_load_data_type_constants;
  ]);
]
