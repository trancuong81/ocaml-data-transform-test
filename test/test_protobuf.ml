let test_string_type_binary_roundtrip () =
  let original =
    Ocaml_data_transform.Data_types.make_string_type ~type_id:"Ssn" ~value:"123-45-6789"
      ~regex:{|^\d{3}-\d{2}-\d{4}$|} ~format_patterns:["000-00-0000"] ()
  in
  let encoder = Pbrt.Encoder.create () in
  Ocaml_data_transform.Data_types.encode_pb_string_type original encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let decoder = Pbrt.Decoder.of_bytes bytes in
  let decoded = Ocaml_data_transform.Data_types.decode_pb_string_type decoder in
  Alcotest.(check string) "type_id" original.type_id decoded.type_id;
  Alcotest.(check string) "value" original.value decoded.value;
  Alcotest.(check string) "regex" original.regex decoded.regex;
  Alcotest.(check (list string)) "format_patterns"
    original.format_patterns decoded.format_patterns

let test_string_type_json_roundtrip () =
  let original =
    Ocaml_data_transform.Data_types.make_string_type ~type_id:"Email" ~value:"test@example.com" ()
  in
  let json = Ocaml_data_transform.Data_types.encode_json_string_type original in
  let decoded = Ocaml_data_transform.Data_types.decode_json_string_type json in
  Alcotest.(check string) "type_id" original.type_id decoded.type_id;
  Alcotest.(check string) "value" original.value decoded.value

let test_multiple_checkbox_roundtrip () =
  let original =
    Ocaml_data_transform.Data_types.make_multiple_checkbox_type ~type_id:"MultipleCheckbox"
      ~selected_keys:["opt1"; "opt3"]
      ~all_option_keys_in_order:["opt1"; "opt2"; "opt3"]
      ~all_option_labels_in_order:["Option 1"; "Option 2"; "Option 3"] ()
  in
  let encoder = Pbrt.Encoder.create () in
  Ocaml_data_transform.Data_types.encode_pb_multiple_checkbox_type original encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let decoder = Pbrt.Decoder.of_bytes bytes in
  let decoded = Ocaml_data_transform.Data_types.decode_pb_multiple_checkbox_type decoder in
  Alcotest.(check (list string)) "selected_keys"
    original.selected_keys decoded.selected_keys

let test_address_binary_roundtrip () =
  let open Ocaml_data_transform.Data_types in
  let city_field = make_string_type ~type_id:"City" ~value:"Springfield" () in
  let sub_fields = make_address_sub_fields ~city:city_field () in
  let original = make_address_type
    ~type_id:"Address"
    ~value_sub_fields:sub_fields
    ~sub_field_keys_in_order:
      ["numberAndStreet"; "city"; "stateProvince"; "country";
       "postalZipCode"; "fullAddress"]
    () in
  let encoder = Pbrt.Encoder.create () in
  encode_pb_address_type original encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let decoder = Pbrt.Decoder.of_bytes bytes in
  let decoded = decode_pb_address_type decoder in
  Alcotest.(check string) "type_id" original.type_id decoded.type_id;
  let decoded_sub = Option.get decoded.value_sub_fields in
  let decoded_city = Option.get decoded_sub.city in
  Alcotest.(check string) "city value" "Springfield" decoded_city.value;
  Alcotest.(check (list string)) "sub_field_keys_in_order"
    original.sub_field_keys_in_order decoded.sub_field_keys_in_order

let test_table_schema_roundtrip () =
  let open Ocaml_data_transform in
  let str_val = Data_types.make_string_type ~type_id:"String" ~value:"hello" () in
  let field = Table_schema.make_single_field_type
    ~value:(Table_schema.String_type str_val)
    ~label:"Test Field"
    () in
  let schema = Table_schema.make_table_schema
    ~fields_map:[("field1", field)]
    ~field_keys_in_order:["field1"]
    ~label:"Test Schema"
    () in
  let encoder = Pbrt.Encoder.create () in
  Table_schema.encode_pb_table_schema schema encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let decoder = Pbrt.Decoder.of_bytes bytes in
  let decoded = Table_schema.decode_pb_table_schema decoder in
  Alcotest.(check string) "label" "Test Schema" decoded.label;
  Alcotest.(check int) "fields_map length" 1 (List.length decoded.fields_map);
  let (key, field_decoded) = List.hd decoded.fields_map in
  Alcotest.(check string) "field key" "field1" key;
  Alcotest.(check string) "field label" "Test Field" field_decoded.label;
  match field_decoded.value with
  | Some (Table_schema.String_type st) ->
    Alcotest.(check string) "string value" "hello" st.value
  | _ -> Alcotest.fail "expected String_type variant"

let suite =
  [
    ( "protobuf",
      [
        Alcotest.test_case "StringType binary roundtrip" `Quick
          test_string_type_binary_roundtrip;
        Alcotest.test_case "StringType JSON roundtrip" `Quick
          test_string_type_json_roundtrip;
        Alcotest.test_case "MultipleCheckboxType roundtrip" `Quick
          test_multiple_checkbox_roundtrip;
        Alcotest.test_case "AddressType roundtrip" `Quick
          test_address_binary_roundtrip;
        Alcotest.test_case "TableSchema roundtrip" `Quick
          test_table_schema_roundtrip;
      ] );
  ]
