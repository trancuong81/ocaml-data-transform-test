module EM = Ocaml_data_transform.Example_mappings
module M = Ocaml_data_transform.Mappings
module JP = Ocaml_data_transform.Json_path

let test_split_name_full () =
  let result = EM.split_name "John Michael Smith" in
  Alcotest.(check string) "first" "John" result.first_name;
  Alcotest.(check string) "middle" "Michael" result.middle_name;
  Alcotest.(check string) "last" "Smith" result.last_name

let test_split_name_two_parts () =
  let result = EM.split_name "Jane Doe" in
  Alcotest.(check string) "first" "Jane" result.first_name;
  Alcotest.(check string) "middle" "" result.middle_name;
  Alcotest.(check string) "last" "Doe" result.last_name

let test_split_name_empty () =
  let result = EM.split_name "" in
  Alcotest.(check string) "first" "" result.first_name;
  Alcotest.(check string) "middle" "" result.middle_name;
  Alcotest.(check string) "last" "" result.last_name

let test_split_name_single () =
  let result = EM.split_name "Madonna" in
  Alcotest.(check string) "first" "Madonna" result.first_name;
  Alcotest.(check string) "last" "Madonna" result.last_name

let test_commitment_mapping () =
  let input = Yojson.Safe.from_string
    {|{"subdoc": {"lp_signatory": {"rawValue": {"asa_commitment_amount": {"rawValue": {"value": "5000000"}}}}}}|}
  in
  let all = EM.all_mappings () in
  let result = M.transform_all all input in
  let value = JP.get_string result ["sf_Agreement_null_Commitment_c"; "rawValue"; "value"] in
  Alcotest.(check (option string)) "commitment" (Some "5000000") value

let suite = [
  ("example_mappings", [
    Alcotest.test_case "split_name full" `Quick test_split_name_full;
    Alcotest.test_case "split_name two parts" `Quick test_split_name_two_parts;
    Alcotest.test_case "split_name empty" `Quick test_split_name_empty;
    Alcotest.test_case "split_name single" `Quick test_split_name_single;
    Alcotest.test_case "commitment mapping" `Quick test_commitment_mapping;
  ]);
]
