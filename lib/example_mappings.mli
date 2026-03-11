type name_parts = {
  first_name : string;
  middle_name : string;
  last_name : string;
}

val split_name : string -> name_parts

val transform :
  Source_table.source_table_fields_map ->
  Target_table.target_table_fields_map
