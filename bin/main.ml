let () =
  let input_path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else (
      Printf.eprintf "Usage: %s <values.json>\n%!" Sys.argv.(0);
      exit 1)
  in
  let source_json = Yojson.Basic.from_file input_path in
  let source = Ocaml_data_transform.Source_table.decode_json_source_table_fields_map source_json in
  let target = Ocaml_data_transform.Example_mappings.transform source in
  let result = Ocaml_data_transform.Target_table.encode_json_target_table_fields_map target in
  Yojson.Basic.pretty_to_channel stdout result;
  print_newline ()
