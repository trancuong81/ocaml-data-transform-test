let identity x = x

let group_by (items : 'a list) (key_fn : 'a -> string) (value_fn : 'a -> 'b)
    : (string * 'b list) list =
  let tbl = Hashtbl.create 16 in
  let order = ref [] in
  List.iter
    (fun item ->
      let k = key_fn item in
      let v = value_fn item in
      (match Hashtbl.find_opt tbl k with
       | None ->
         order := k :: !order;
         Hashtbl.add tbl k [v]
       | Some existing -> Hashtbl.replace tbl k (existing @ [v])))
    items;
  List.rev !order |> List.rev_map (fun k -> (k, Hashtbl.find tbl k)) |> List.rev

let merge_by (items : 'a list) (key_fn : 'a -> string) (value_fn : 'a -> 'b)
    : (string * 'b) list =
  let tbl = Hashtbl.create 16 in
  let order = ref [] in
  List.iter
    (fun item ->
      let k = key_fn item in
      if not (Hashtbl.mem tbl k) then order := k :: !order;
      Hashtbl.replace tbl k (value_fn item))
    items;
  List.rev !order |> List.rev_map (fun k -> (k, Hashtbl.find tbl k)) |> List.rev

let map_values (pairs : (string * 'a) list) (f : string -> 'a -> 'b)
    : (string * 'b) list =
  List.rev_map (fun (k, v) -> (k, f k v)) pairs |> List.rev

let rec deep_merge (objects : Yojson.Safe.t list) : Yojson.Safe.t =
  List.fold_left deep_merge_two (`Assoc []) objects

and deep_merge_two (acc : Yojson.Safe.t) (obj : Yojson.Safe.t) : Yojson.Safe.t =
  match (acc, obj) with
  | `Assoc acc_fields, `Assoc obj_fields ->
    let merged =
      List.fold_left
        (fun fields (key, value) ->
          let without_key = List.filter (fun (k, _) -> k <> key) fields in
          match List.assoc_opt key fields with
          | Some (`Assoc _ as existing) when (match value with `Assoc _ -> true | _ -> false) ->
            let updated = deep_merge_two existing value in
            without_key @ [(key, updated)]
          | _ ->
            without_key @ [(key, value)])
        acc_fields obj_fields
    in
    `Assoc merged
  | _, other -> other
