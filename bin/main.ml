let () =
  let constants = Ocaml_data_transform.Schema.load_data_type_constants () in
  let count = Ocaml_data_transform.Schema.type_count constants in
  Printf.printf "ocaml-data-transform: loaded %d type definitions\n%!" count
