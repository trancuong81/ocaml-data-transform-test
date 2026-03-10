let json_testable =
  Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let test_get_path_simple () =
  let json = Yojson.Safe.from_string {|{"a": {"b": {"c": 42}}}|} in
  let result = Ocaml_data_transform.Json_path.get_path json ["a"; "b"; "c"] in
  Alcotest.(check (option json_testable)) "nested int"
    (Some (`Int 42)) result

let test_get_path_missing () =
  let json = Yojson.Safe.from_string {|{"a": {"b": 1}}|} in
  let result = Ocaml_data_transform.Json_path.get_path json ["a"; "x"] in
  Alcotest.(check (option json_testable)) "missing key"
    None result

let test_get_path_empty () =
  let json = Yojson.Safe.from_string {|{"a": 1}|} in
  let result = Ocaml_data_transform.Json_path.get_path json [] in
  Alcotest.(check (option json_testable)) "empty path"
    (Some json) result

let test_set_path () =
  let json = Yojson.Safe.from_string {|{}|} in
  let result = Ocaml_data_transform.Json_path.set_path json
    ["a"; "b"] (`String "hello") in
  let expected = Yojson.Safe.from_string {|{"a": {"b": "hello"}}|} in
  Alcotest.(check json_testable) "set nested"
    expected result

let suite = [
  ("json_path", [
    Alcotest.test_case "get nested path" `Quick test_get_path_simple;
    Alcotest.test_case "get missing path" `Quick test_get_path_missing;
    Alcotest.test_case "get empty path" `Quick test_get_path_empty;
    Alcotest.test_case "set nested path" `Quick test_set_path;
  ]);
]
