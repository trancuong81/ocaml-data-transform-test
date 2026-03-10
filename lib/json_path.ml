let get_path (json : Yojson.Safe.t) (path : string list) : Yojson.Safe.t option =
  List.fold_left
    (fun acc key ->
      match acc with
      | Some (`Assoc fields) -> List.assoc_opt key fields
      | _ -> None)
    (Some json) path

let set_path (json : Yojson.Safe.t) (path : string list) (value : Yojson.Safe.t)
    : Yojson.Safe.t =
  let rec aux current = function
    | [] -> value
    | [key] ->
      let fields = match current with `Assoc fs -> fs | _ -> [] in
      let updated = List.filter (fun (k, _) -> k <> key) fields in
      `Assoc (updated @ [(key, value)])
    | key :: rest ->
      let fields = match current with `Assoc fs -> fs | _ -> [] in
      let child = match List.assoc_opt key fields with Some v -> v | None -> `Assoc [] in
      let updated_child = aux child rest in
      let updated = List.filter (fun (k, _) -> k <> key) fields in
      `Assoc (updated @ [(key, updated_child)])
  in
  aux json path

let get_string (json : Yojson.Safe.t) (path : string list) : string option =
  match get_path json path with
  | Some (`String s) -> Some s
  | _ -> None

let get_string_or_empty (json : Yojson.Safe.t) (path : string list) : string =
  Option.value ~default:"" (get_string json path)

let get_string_list (json : Yojson.Safe.t) (path : string list) : string list =
  match get_path json path with
  | Some (`List items) ->
    List.filter_map
      (function `String s -> Some s | _ -> None)
      items
  | _ -> []
