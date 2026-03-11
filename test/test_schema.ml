let test_load_data_type_constants () =
  let constants = Ocaml_data_transform.Schema.load_data_type_constants () in
  (* Verify Ssn has correct regex *)
  let ssn = Ocaml_data_transform.Schema.find_type_constants constants "Ssn" in
  Alcotest.(check bool) "Ssn found" true (Option.is_some ssn);
  let ssn = Option.get ssn in
  Alcotest.(check bool) "Ssn has regex" true
    (Ocaml_data_transform.Schema.has_regex ssn);
  (* Verify total count *)
  let count = Ocaml_data_transform.Schema.type_count constants in
  Alcotest.(check int) "47 types" 47 count

let test_load_source_table_schema () =
  let schema = Ocaml_data_transform.Schema.load_source_table_schema () in
  Alcotest.(check string) "label" "subdoc" schema.label;
  Alcotest.(check int) "field_keys_in_order count" 7
    (List.length schema.field_keys_in_order);
  let fm = Option.get schema.fields_map in
  (* Check a StringType field has typeId *)
  let str_field = Option.get fm.asa_fullname_investorname_generalinfo1 in
  Alcotest.(check string) "string field typeId" "String" str_field.type_id;
  (* Check lp_signatory compound field *)
  let sig_field = Option.get fm.lp_signatory in
  Alcotest.(check string) "lp_signatory typeId" "CustomCompound" sig_field.type_id;
  Alcotest.(check int) "lp_signatory sub_field_keys count" 3
    (List.length sig_field.sub_field_keys_in_order);
  (* Check sub-field is typed *)
  let sub_fields = Option.get sig_field.value_sub_fields in
  let commitment_sub = Option.get sub_fields.asa_commitment_amount in
  Alcotest.(check string) "sub-field typeId" "String" commitment_sub.type_id;
  (* Check MultipleCheckbox has option keys *)
  let checkbox = Option.get fm.luxsentity_regulatedstatus_part2_duediligencequestionnaire in
  Alcotest.(check string) "checkbox typeId" "MultipleCheckbox" checkbox.type_id;
  Alcotest.(check int) "checkbox option keys count" 2
    (List.length checkbox.all_option_keys_in_order);
  Alcotest.(check int) "checkbox option labels count" 2
    (List.length checkbox.all_option_labels_in_order)

let test_load_target_table_schema () =
  let schema = Ocaml_data_transform.Schema.load_target_table_schema () in
  Alcotest.(check string) "label" "target" schema.label;
  Alcotest.(check int) "field_keys_in_order count" 8
    (List.length schema.field_keys_in_order);
  let fm = Option.get schema.fields_map in
  (* Check MoneyType field *)
  let money_field = Option.get fm.sf_agreement_null_commitment_c in
  Alcotest.(check string) "commitment typeId" "Money" money_field.type_id;
  (* Check RadioGroupType field *)
  let radio_field = Option.get fm.sf_tax_form_w9_us_tin_type_c in
  Alcotest.(check string) "tin type typeId" "RadioGroup" radio_field.type_id;
  (* Check StringType field *)
  let str_field = Option.get fm.sf_agreement_null_signer_first_name in
  Alcotest.(check string) "signer first name typeId" "String" str_field.type_id;
  (* Check RadioGroup has option keys *)
  Alcotest.(check (list string)) "tin type options" ["SSN"; "EIN"]
    radio_field.all_option_keys_in_order;
  (* Check MultipleCheckbox has option keys *)
  let mc_field = Option.get fm.sf_agreement_null_wlc_international_supplements_c in
  Alcotest.(check int) "intl supplements option count" 6
    (List.length mc_field.all_option_keys_in_order)

let suite = [
  ("schema", [
    Alcotest.test_case "load data type constants" `Quick
      test_load_data_type_constants;
    Alcotest.test_case "load source table schema" `Quick
      test_load_source_table_schema;
    Alcotest.test_case "load target table schema" `Quick
      test_load_target_table_schema;
  ]);
]
