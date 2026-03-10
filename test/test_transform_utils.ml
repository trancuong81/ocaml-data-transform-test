module Utils = Ocaml_data_transform.Transform_utils

let json_testable =
  Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let test_identity () =
  Alcotest.(check int) "identity int" 42 (Utils.identity 42);
  Alcotest.(check string) "identity string" "hello" (Utils.identity "hello")

let test_group_by () =
  let items = [("a", 1); ("b", 2); ("a", 3)] in
  let result = Utils.group_by items fst snd in
  let a_values = List.assoc "a" result in
  Alcotest.(check (list int)) "group a" [1; 3] a_values;
  let b_values = List.assoc "b" result in
  Alcotest.(check (list int)) "group b" [2] b_values

let test_merge_by () =
  let items = [("a", 1); ("b", 2); ("c", 3)] in
  let result = Utils.merge_by items fst snd in
  Alcotest.(check int) "merge a" 1 (List.assoc "a" result);
  Alcotest.(check int) "merge c" 3 (List.assoc "c" result)

let test_map_values () =
  let pairs = [("x", 1); ("y", 2)] in
  let result = Utils.map_values pairs (fun _k v -> v * 10) in
  Alcotest.(check int) "map x" 10 (List.assoc "x" result);
  Alcotest.(check int) "map y" 20 (List.assoc "y" result)

let test_deep_merge () =
  let a = Yojson.Safe.from_string {|{"x": 1, "nested": {"a": 1}}|} in
  let b = Yojson.Safe.from_string {|{"y": 2, "nested": {"b": 2}}|} in
  let result = Utils.deep_merge [a; b] in
  let expected = Yojson.Safe.from_string
    {|{"x": 1, "y": 2, "nested": {"a": 1, "b": 2}}|} in
  Alcotest.(check json_testable) "deep merge" expected result

let test_deep_merge_overwrite () =
  let a = Yojson.Safe.from_string {|{"x": 1}|} in
  let b = Yojson.Safe.from_string {|{"x": 2}|} in
  let result = Utils.deep_merge [a; b] in
  let expected = Yojson.Safe.from_string {|{"x": 2}|} in
  Alcotest.(check json_testable) "overwrite" expected result

let suite = [
  ("transform_utils", [
    Alcotest.test_case "identity" `Quick test_identity;
    Alcotest.test_case "group_by" `Quick test_group_by;
    Alcotest.test_case "merge_by" `Quick test_merge_by;
    Alcotest.test_case "map_values" `Quick test_map_values;
    Alcotest.test_case "deep_merge" `Quick test_deep_merge;
    Alcotest.test_case "deep_merge overwrite" `Quick test_deep_merge_overwrite;
  ]);
]
