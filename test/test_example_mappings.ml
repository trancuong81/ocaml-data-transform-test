module EM = Ocaml_data_transform.Example_mappings
module DT = Ocaml_data_transform.Data_types
module ST = Ocaml_data_transform.Source_table
module TT = Ocaml_data_transform.Target_table

let test_split_name_full () =
  let result = EM.split_name "John Michael Smith" in
  Alcotest.(check string) "first" "John" result.first_name;
  Alcotest.(check string) "middle" "Michael" result.middle_name;
  Alcotest.(check string) "last" "Smith" result.last_name

let test_split_name_two_parts () =
  let result = EM.split_name "Jane Doe" in
  Alcotest.(check string) "first" "Jane" result.first_name;
  Alcotest.(check string) "middle" "" result.middle_name;
  Alcotest.(check string) "last" "Doe" result.last_name

let test_split_name_empty () =
  let result = EM.split_name "" in
  Alcotest.(check string) "first" "" result.first_name;
  Alcotest.(check string) "middle" "" result.middle_name;
  Alcotest.(check string) "last" "" result.last_name

let test_split_name_single () =
  let result = EM.split_name "Madonna" in
  Alcotest.(check string) "first" "Madonna" result.first_name;
  Alcotest.(check string) "last" "Madonna" result.last_name

let make_source
    ?(commitment = "5000000")
    ?(ind_name = "")
    ?(ent_name = "John Doe")
    ?(aml_name = "")
    ?(general_name = "ACME Corp")
    ?(regulated_keys = ["yes_luxsentity_regulatedstatus_part2_duediligencequestionnaire"])
    ?(indi_intl_keys = [])
    ?(entity_intl_keys = [])
    ?(ssn = "")
    ?(ein = "")
    ?(w9_line2 = "")
    () : ST.source_table_fields_map =
  let lp_fields = ST.make_lp_signatory_fields
    ~asa_commitment_amount:(DT.make_string_type ~type_id:"String" ~value:commitment ())
    ~individual_subscribername_signaturepage:(DT.make_string_type ~type_id:"String" ~value:ind_name ())
    ~entity_authorizedname_signaturepage:(DT.make_string_type ~type_id:"String" ~value:ent_name ())
    ()
  in
  let lp = ST.make_lp_signatory_type ~type_id:"CustomCompound" ~value_sub_fields:lp_fields () in
  let w9_fields = ST.make_w9_fields
    ~w9_parti_ssn1:(DT.make_string_type ~type_id:"String" ~value:ssn ())
    ~w9_parti_ein1:(DT.make_string_type ~type_id:"String" ~value:ein ())
    ~w9_line2:(DT.make_string_type ~type_id:"String" ~value:w9_line2 ())
    ()
  in
  let w9 = ST.make_w9_type ~type_id:"CustomCompound" ~value_sub_fields:w9_fields () in
  ST.make_source_table_fields_map
    ~lp_signatory:lp
    ~asa_fullname_investorname_amlquestionnaire:
      (DT.make_string_type ~type_id:"String" ~value:aml_name ())
    ~asa_fullname_investorname_generalinfo1:
      (DT.make_string_type ~type_id:"String" ~value:general_name ())
    ~luxsentity_regulatedstatus_part2_duediligencequestionnaire:
      (DT.make_multiple_checkbox_type ~type_id:"MultipleCheckbox" ~selected_keys:regulated_keys ())
    ~indi_internationalsupplements_part1_duediligencequestionnaire:
      (DT.make_multiple_checkbox_type ~type_id:"MultipleCheckbox" ~selected_keys:indi_intl_keys ())
    ~entity_internationalsupplements_part1_duediligencequestionnaire:
      (DT.make_multiple_checkbox_type ~type_id:"MultipleCheckbox" ~selected_keys:entity_intl_keys ())
    ~w9
    ()

let test_commitment_mapping () =
  let src = make_source ~commitment:"1,000,000" () in
  let target = EM.transform src in
  let money = Option.get target.sf_agreement_null_commitment_c in
  let sub = Option.get money.value_sub_fields in
  let amount = Option.get sub.amount in
  Alcotest.(check (float 0.01)) "commitment amount" 1000000.0 amount.value

let test_investor_name_prefers_aml () =
  let src = make_source ~aml_name:"AML Name" ~general_name:"General Name" () in
  let target = EM.transform src in
  let name_field = Option.get target.sf_account_subscription_investor_name in
  Alcotest.(check string) "investor name" "AML Name" name_field.value

let test_investor_name_falls_back_to_general () =
  let src = make_source ~aml_name:"" ~general_name:"General Name" () in
  let target = EM.transform src in
  let name_field = Option.get target.sf_account_subscription_investor_name in
  Alcotest.(check string) "investor name" "General Name" name_field.value

let test_regulated_status_yes () =
  let src = make_source
    ~regulated_keys:["yes_luxsentity_regulatedstatus_part2_duediligencequestionnaire"]
    ()
  in
  let target = EM.transform src in
  let radio = Option.get target.sf_account_subscription_investor_wlc_publicly_listed_on_a_stock_exchange_c in
  Alcotest.(check string) "regulated status" "true" radio.selected_key

let test_regulated_status_no () =
  let src = make_source
    ~regulated_keys:["no_luxsentity_regulatedstatus_part2_duediligencequestionnaire"]
    ()
  in
  let target = EM.transform src in
  let radio = Option.get target.sf_account_subscription_investor_wlc_publicly_listed_on_a_stock_exchange_c in
  Alcotest.(check string) "regulated status" "false" radio.selected_key

let test_international_supplements () =
  let src = make_source
    ~entity_intl_keys:["none_entity_internationalsupplements_part1_duediligencequestionnaire"]
    ()
  in
  let target = EM.transform src in
  let mc = Option.get target.sf_agreement_null_wlc_international_supplements_c in
  Alcotest.(check (list string)) "intl supplements" ["No Supplement"] mc.selected_keys

let test_signer_name_split () =
  let src = make_source ~ent_name:"Catherine L Ziobro" ~ind_name:"" () in
  let target = EM.transform src in
  let first = Option.get target.sf_agreement_null_signer_first_name in
  let middle = Option.get target.sf_agreement_null_signer_middle_name in
  let last = Option.get target.sf_agreement_null_signer_last_name in
  Alcotest.(check string) "first" "Catherine" first.value;
  Alcotest.(check string) "middle" "L" middle.value;
  Alcotest.(check string) "last" "Ziobro" last.value

let test_signer_name_prefers_individual () =
  let src = make_source ~ind_name:"Jane Smith" ~ent_name:"Entity Auth" () in
  let target = EM.transform src in
  let first = Option.get target.sf_agreement_null_signer_first_name in
  Alcotest.(check string) "first from individual" "Jane" first.value

let test_w9_tin_type_ein () =
  let src = make_source ~ssn:"" ~ein:"" () in
  let target = EM.transform src in
  let radio = Option.get target.sf_tax_form_w9_us_tin_type_c in
  Alcotest.(check string) "tin type" "EIN" radio.selected_key

let test_w9_tin_type_ssn () =
  let src = make_source ~ssn:"123-45-6789" () in
  let target = EM.transform src in
  let radio = Option.get target.sf_tax_form_w9_us_tin_type_c in
  Alcotest.(check string) "tin type" "SSN" radio.selected_key

let test_w9_tin_type_line2_present () =
  let src = make_source ~w9_line2:"Some LLC" () in
  let target = EM.transform src in
  let radio = Option.get target.sf_tax_form_w9_us_tin_type_c in
  Alcotest.(check string) "tin type empty when line2 present" "" radio.selected_key

let suite = [
  ("example_mappings", [
    Alcotest.test_case "split_name full" `Quick test_split_name_full;
    Alcotest.test_case "split_name two parts" `Quick test_split_name_two_parts;
    Alcotest.test_case "split_name empty" `Quick test_split_name_empty;
    Alcotest.test_case "split_name single" `Quick test_split_name_single;
    Alcotest.test_case "commitment mapping" `Quick test_commitment_mapping;
    Alcotest.test_case "investor name prefers aml" `Quick test_investor_name_prefers_aml;
    Alcotest.test_case "investor name falls back" `Quick test_investor_name_falls_back_to_general;
    Alcotest.test_case "regulated status yes" `Quick test_regulated_status_yes;
    Alcotest.test_case "regulated status no" `Quick test_regulated_status_no;
    Alcotest.test_case "international supplements" `Quick test_international_supplements;
    Alcotest.test_case "signer name split" `Quick test_signer_name_split;
    Alcotest.test_case "signer name prefers individual" `Quick test_signer_name_prefers_individual;
    Alcotest.test_case "w9 tin type ein" `Quick test_w9_tin_type_ein;
    Alcotest.test_case "w9 tin type ssn" `Quick test_w9_tin_type_ssn;
    Alcotest.test_case "w9 tin type line2" `Quick test_w9_tin_type_line2_present;
  ]);
]
