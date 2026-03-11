module M = Ocaml_data_transform.Mappings
module EM = Ocaml_data_transform.Example_mappings

let find_fixture name =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat (Filename.concat root "test/fixtures") name
  | None -> Filename.concat "test/fixtures" name

let rec normalize_json (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc fields ->
    `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b)
      (List.map (fun (k, v) -> (k, normalize_json v)) fields))
  | `List items -> `List (List.map normalize_json items)
  | other -> other

let test_values_json_pipeline () =
  let values_path = find_fixture "values.json" in
  let expected_path = find_fixture "transformed_values.json" in
  if not (Sys.file_exists values_path) then
    Alcotest.skip ()
  else begin
    let input = Yojson.Safe.from_file values_path in
    let mappings = EM.all_mappings () in
    let result = M.transform_all mappings input in
    let expected = Yojson.Safe.from_file expected_path in
    let result_normalized = normalize_json result in
    let expected_normalized = normalize_json expected in
    Alcotest.(check string) "transform output matches transformed_values.json"
      (Yojson.Safe.pretty_to_string expected_normalized)
      (Yojson.Safe.pretty_to_string result_normalized)
  end

let suite = [
  ("integration", [
    Alcotest.test_case "values.json pipeline" `Quick
      test_values_json_pipeline;
  ]);
]
