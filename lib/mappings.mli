type input_info = (string * string list) list
type mapping

val textbox_mapping :
  name:string ->
  input_paths:input_info ->
  output_path:string list ->
  mapping

val checkbox_mapping :
  name:string ->
  input_paths:input_info ->
  output_path:string list ->
  option_map:(string * string) list ->
  mapping

val custom_mapping :
  name:string ->
  input_paths:input_info ->
  output_path:string list ->
  transform_fn:(Yojson.Safe.t -> Yojson.Safe.t) ->
  mapping

val mapping_name : mapping -> string
val mapping_output_path : mapping -> string list
val apply_mapping : mapping -> Yojson.Safe.t -> Yojson.Safe.t
val transform_all : mapping list -> Yojson.Safe.t -> Yojson.Safe.t
