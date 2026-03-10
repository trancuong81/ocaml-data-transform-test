module JP = Json_path

type input_info = (string * string list) list

type transform_fn = Yojson.Safe.t -> Yojson.Safe.t

type mapping = {
  name : string;
  input_paths : input_info;
  output_path : string list;
  transform : transform_fn;
}

let get_value (json : Yojson.Safe.t) (path : string list) : Yojson.Safe.t =
  match JP.get_path json path with
  | Some v -> v
  | None -> `Null

let build_output (value : Yojson.Safe.t) (path : string list) : Yojson.Safe.t =
  List.fold_right
    (fun key acc ->
      if key = "" then acc
      else `Assoc [(key, acc)])
    path value

let gather_inputs (json : Yojson.Safe.t) (input_paths : input_info) : Yojson.Safe.t =
  let pairs =
    List.rev_map
      (fun (alias, path) -> (alias, get_value json path))
      input_paths
    |> List.rev
  in
  `Assoc pairs

let textbox_mapping ~name ~input_paths ~output_path : mapping =
  let transform input_json =
    let values =
      match input_json with
      | `Assoc fields ->
        List.filter_map
          (fun (_alias, v) ->
            match JP.get_path v ["rawValue"; "value"] with
            | Some (`String s) when s <> "" -> Some s
            | _ -> None)
          fields
      | _ -> []
    in
    let joined = String.concat "" values in
    build_output
      (`Assoc [("rawValue", `Assoc [("value", `String joined)])])
      output_path
  in
  { name; input_paths; output_path; transform }

let checkbox_mapping ~name ~input_paths ~output_path ~option_map : mapping =
  let transform input_json =
    let selected_keys =
      match input_json with
      | `Assoc fields ->
        List.concat_map
          (fun (_alias, v) ->
            match v with
            | `Assoc _ ->
              (match JP.get_path v ["selectedKeys"] with
               | Some (`List keys) ->
                 List.filter_map
                   (function
                     | `String k -> List.assoc_opt k option_map
                     | _ -> None)
                   keys
               | _ -> [])
            | _ -> [])
          fields
      | _ -> []
    in
    let unique =
      List.fold_left
        (fun acc key -> if List.mem key acc then acc else key :: acc)
        [] selected_keys
      |> List.rev
    in
    build_output
      (`Assoc [("selectedKeys", `List (List.map (fun s -> `String s) unique))])
      output_path
  in
  { name; input_paths; output_path; transform }

let custom_mapping ~name ~input_paths ~output_path ~transform_fn : mapping =
  let transform input_json =
    let result = transform_fn input_json in
    build_output result output_path
  in
  { name; input_paths; output_path; transform }

let mapping_name (m : mapping) = m.name
let mapping_output_path (m : mapping) = m.output_path

let apply_mapping (mapping : mapping) (source : Yojson.Safe.t) : Yojson.Safe.t =
  let input = gather_inputs source mapping.input_paths in
  mapping.transform input

let transform_all (mappings : mapping list) (source : Yojson.Safe.t) : Yojson.Safe.t =
  let results = List.rev_map (fun m -> apply_mapping m source) mappings |> List.rev in
  Transform_utils.deep_merge results
