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
  let original =
    Ocaml_data_transform.Data_types.make_address_type ~type_id:"Address"
      ~number_and_street:"123 Main St" ~city:"Springfield"
      ~state_province:"IL" ~country:"US" ~postal_zip_code:"62701"
      ~full_address:"123 Main St, Springfield, IL 62701"
      ~sub_field_keys_in_order:
        [ "numberAndStreet"; "city"; "stateProvince"; "country";
          "postalZipCode"; "fullAddress" ]
      ()
  in
  let encoder = Pbrt.Encoder.create () in
  Ocaml_data_transform.Data_types.encode_pb_address_type original encoder;
  let bytes = Pbrt.Encoder.to_bytes encoder in
  let decoder = Pbrt.Decoder.of_bytes bytes in
  let decoded = Ocaml_data_transform.Data_types.decode_pb_address_type decoder in
  Alcotest.(check string) "city" original.city decoded.city;
  Alcotest.(check string) "type_id" original.type_id decoded.type_id;
  Alcotest.(check (list string)) "sub_field_keys_in_order"
    original.sub_field_keys_in_order decoded.sub_field_keys_in_order

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
      ] );
  ]
