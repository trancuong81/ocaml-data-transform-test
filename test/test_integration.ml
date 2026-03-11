module EM = Ocaml_data_transform.Example_mappings
module ST = Ocaml_data_transform.Source_table
module TT = Ocaml_data_transform.Target_table

let find_fixture name =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat (Filename.concat root "test/fixtures") name
  | None -> Filename.concat "test/fixtures" name

let rec normalize_json (json : Yojson.Basic.t) : Yojson.Basic.t =
  match json with
  | `Assoc fields ->
    `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b)
      (List.map (fun (k, v) -> (k, normalize_json v)) fields))
  | `List items -> `List (List.map normalize_json items)
  | other -> other

let test_typed_pipeline () =
  let values_path = find_fixture "values.json" in
  let expected_path = find_fixture "transformed_values.json" in
  if not (Sys.file_exists values_path) then
    Alcotest.skip ()
  else begin
    let source_json = Yojson.Basic.from_file values_path in
    let source = ST.decode_json_source_table_fields_map source_json in
    let target = EM.transform source in
    let result_json = TT.encode_json_target_table_fields_map target in
    let expected_json = Yojson.Basic.from_file expected_path in
    let expected = TT.decode_json_target_table_fields_map expected_json in
    let expected_reencoded = TT.encode_json_target_table_fields_map expected in
    let result_str = Yojson.Basic.pretty_to_string (normalize_json result_json) in
    let expected_str = Yojson.Basic.pretty_to_string (normalize_json expected_reencoded) in
    Alcotest.(check string) "typed transform output matches expected"
      expected_str result_str
  end

let suite = [
  ("integration", [
    Alcotest.test_case "typed pipeline" `Quick
      test_typed_pipeline;
  ]);
]
