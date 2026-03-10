val get_path : Yojson.Safe.t -> string list -> Yojson.Safe.t option
val set_path : Yojson.Safe.t -> string list -> Yojson.Safe.t -> Yojson.Safe.t
val get_string : Yojson.Safe.t -> string list -> string option
val get_string_or_empty : Yojson.Safe.t -> string list -> string
val get_string_list : Yojson.Safe.t -> string list -> string list
