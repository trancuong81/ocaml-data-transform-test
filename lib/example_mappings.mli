type name_parts = {
  first_name : string;
  middle_name : string;
  last_name : string;
}

val split_name : string -> name_parts
val all_mappings : unit -> Mappings.mapping list
