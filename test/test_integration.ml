module M = Ocaml_data_transform.Mappings
module EM = Ocaml_data_transform.Example_mappings

let find_fixture name =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat (Filename.concat root "test/fixtures") name
  | None -> Filename.concat "test/fixtures" name

let test_values_json_pipeline () =
  let values_path = find_fixture "values.json" in
  if not (Sys.file_exists values_path) then
    Alcotest.skip ()
  else begin
    let input = Yojson.Safe.from_file values_path in
    let mappings = EM.all_mappings () in
    let result = M.transform_all mappings input in
    (match result with
     | `Assoc _ -> ()
     | _ -> Alcotest.fail "Expected JSON object output");
    Printf.printf "Integration test passed with %d output keys\n%!"
      (match result with `Assoc fields -> List.length fields | _ -> 0)
  end

let suite = [
  ("integration", [
    Alcotest.test_case "values.json pipeline" `Quick
      test_values_json_pipeline;
  ]);
]
