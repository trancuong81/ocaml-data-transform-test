module SMap = Map.Make (String)

type type_constants = {
  type_id : string;
  regex : string option;
  format_patterns : string list;
  options : string list;
  min_value : float option;
  max_value : float option;
  sub_field_keys_in_order : string list;
}

type data_type_constants = type_constants SMap.t

let json_string_list json =
  let open Yojson.Safe.Util in
  json |> to_list |> List.map to_string

let parse_type_constants (json : Yojson.Safe.t) : type_constants =
  let open Yojson.Safe.Util in
  {
    type_id = json |> member "typeId" |> to_string;
    regex = json |> member "regex" |> to_string_option;
    format_patterns =
      (match json |> member "formatPatterns" with
       | `Null -> []
       | j -> json_string_list j);
    options =
      (match json |> member "options" with
       | `Null -> []
       | j -> json_string_list j);
    min_value =
      (match json |> member "min" with
       | `Null -> None
       | `Int n -> Some (Float.of_int n)
       | `Float f -> Some f
       | _ -> None);
    max_value =
      (match json |> member "max" with
       | `Null -> None
       | `Int n -> Some (Float.of_int n)
       | `Float f -> Some f
       | _ -> None);
    sub_field_keys_in_order =
      (match json |> member "subFieldKeysInOrder" with
       | `Null -> []
       | j -> json_string_list j);
  }

(* Locate proto/ directory. During dune runtest, cwd is _build/default/test/
   so we need to walk up to find the project root. *)
let find_proto_dir () =
  (* Prefer DUNE_SOURCEROOT when available — dune sets this and it always
     points to the real source tree (not _build/) where JSON files live. *)
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root ->
    let dir = Filename.concat root "proto" in
    if Sys.file_exists dir && Sys.is_directory dir then dir
    else failwith ("DUNE_SOURCEROOT/proto not found: " ^ dir)
  | None ->
    let candidates =
      [
        "proto";
        "../proto";
        "../../proto";
        "../../../proto";
        Filename.concat (Sys.getcwd ()) "proto";
      ]
    in
    (match
       List.find_opt (fun d -> Sys.file_exists d && Sys.is_directory d) candidates
     with
     | Some dir -> dir
     | None ->
       failwith
         "Cannot find proto/ directory. Set DUNE_SOURCEROOT or run from \
          project root.")

let find_tables_dir () =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root ->
    let dir = Filename.concat (Filename.concat root "proto") "tables" in
    if Sys.file_exists dir && Sys.is_directory dir then dir
    else failwith ("DUNE_SOURCEROOT/proto/tables not found: " ^ dir)
  | None ->
    let candidates =
      [
        "proto/tables";
        "../proto/tables";
        "../../proto/tables";
      ]
    in
    (match
       List.find_opt (fun d -> Sys.file_exists d && Sys.is_directory d) candidates
     with
     | Some dir -> dir
     | None ->
       failwith
         "Cannot find proto/tables/ directory. Set DUNE_SOURCEROOT or run from project root.")

let load_data_type_constants () : data_type_constants =
  let path = Filename.concat (find_proto_dir ()) "data_types_constants.json" in
  let json = Yojson.Safe.from_file path in
  let open Yojson.Safe.Util in
  json |> to_assoc
  |> List.fold_left
       (fun acc (key, value) -> SMap.add key (parse_type_constants value) acc)
       SMap.empty

let find_type_constants (m : data_type_constants) (type_id : string) =
  SMap.find_opt type_id m

let has_regex (tc : type_constants) = Option.is_some tc.regex
let type_count (m : data_type_constants) = SMap.cardinal m

(* Convert snake_case JSON keys to lowerCamelCase recursively.
   ocaml-protoc only accepts camelCase in JSON, but we want constants files
   to use snake_case field names matching the proto definitions. *)
let snake_to_camel (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let capitalize_next = ref false in
  String.iter (fun c ->
    if c = '_' then
      capitalize_next := true
    else if !capitalize_next then begin
      Buffer.add_char buf (Char.uppercase_ascii c);
      capitalize_next := false
    end else
      Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let rec camelcase_keys (json : Yojson.Basic.t) : Yojson.Basic.t =
  match json with
  | `Assoc pairs ->
    `Assoc (List.map (fun (k, v) -> (snake_to_camel k, camelcase_keys v)) pairs)
  | `List items -> `List (List.map camelcase_keys items)
  | other -> other

let load_source_table_schema () : Source_table.source_table_schema =
  let path = Filename.concat (find_tables_dir ()) "source_table_constants.json" in
  let json = Yojson.Basic.from_file path |> camelcase_keys in
  Source_table.decode_json_source_table_schema json

let load_target_table_schema () : Target_table.target_table_schema =
  let path = Filename.concat (find_tables_dir ()) "target_table_constants.json" in
  let json = Yojson.Basic.from_file path |> camelcase_keys in
  Target_table.decode_json_target_table_schema json

