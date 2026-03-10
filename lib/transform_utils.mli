val identity : 'a -> 'a
val group_by : 'a list -> ('a -> string) -> ('a -> 'b) -> (string * 'b list) list
val merge_by : 'a list -> ('a -> string) -> ('a -> 'b) -> (string * 'b) list
val map_values : (string * 'a) list -> (string -> 'a -> 'b) -> (string * 'b) list
val deep_merge : Yojson.Safe.t list -> Yojson.Safe.t
