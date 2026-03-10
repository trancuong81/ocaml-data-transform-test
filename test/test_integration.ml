module M = Ocaml_data_transform.Mappings
module EM = Ocaml_data_transform.Example_mappings

let test_values_json_pipeline () =
  let values_path = "/Users/cuongtran/w/jsonata/values.json" in
  if not (Sys.file_exists values_path) then
    Alcotest.skip ()
  else begin
    let input = Yojson.Safe.from_file values_path in
    let mappings = EM.all_mappings () in
    let result = M.transform_all mappings input in
    (match result with
     | `Assoc _ -> ()
     | _ -> Alcotest.fail "Expected JSON object output");
    let output_path = "/tmp/ocaml_transform_output.json" in
    Yojson.Safe.to_file ~std:true output_path result;
    Printf.printf "Output written to %s\n%!" output_path
  end

let suite = [
  ("integration", [
    Alcotest.test_case "values.json pipeline" `Quick
      test_values_json_pipeline;
  ]);
]
