let () =
  let input_path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else (
      Printf.eprintf "Usage: %s <values.json>\n%!" Sys.argv.(0);
      exit 1)
  in
  let input = Yojson.Safe.from_file input_path in
  let mappings = Ocaml_data_transform.Example_mappings.all_mappings () in
  let result = Ocaml_data_transform.Mappings.transform_all mappings input in
  Yojson.Safe.pretty_to_channel stdout result;
  print_newline ()
