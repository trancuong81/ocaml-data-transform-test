module SMap : Map.S with type key = string

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
type table_schema_constants

val load_data_type_constants : unit -> data_type_constants
val single_value_type_ids : table_schema_constants -> string list
val compound_type_ids : table_schema_constants -> string list
val find_type_constants : data_type_constants -> string -> type_constants option
val has_regex : type_constants -> bool
val type_count : data_type_constants -> int
val load_table_schema_constants : unit -> table_schema_constants
val all_type_ids : table_schema_constants -> string list
