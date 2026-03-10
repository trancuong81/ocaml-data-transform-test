module M = Ocaml_data_transform.Mappings
module JP = Ocaml_data_transform.Json_path

let test_textbox_mapping () =
  let input = Yojson.Safe.from_string
    {|{"subdoc": {"lp_signatory": {"rawValue": {"asa_commitment_amount": {"rawValue": {"value": "1000000"}}}}}}|}
  in
  let mapping = M.textbox_mapping
    ~name:"sf_Agreement_null_Commitment_c"
    ~input_paths:[
      ("subdoc", ["subdoc"; "lp_signatory"; "rawValue"; "asa_commitment_amount"]);
    ]
    ~output_path:["sf_Agreement_null_Commitment_c"]
  in
  let result = M.apply_mapping mapping input in
  let value = JP.get_string result ["sf_Agreement_null_Commitment_c"; "rawValue"; "value"] in
  Alcotest.(check (option string)) "commitment value"
    (Some "1000000") value

let test_checkbox_mapping () =
  let input = Yojson.Safe.from_string
    {|{"subdoc": {"luxsentity_regulatedstatus_part2_duediligencequestionnaire":
        {"rawValue": {"selectedKeys": ["yes_luxsentity_regulatedstatus_part2_duediligencequestionnaire"]}}}}|}
  in
  let option_map = [
    ("yes_luxsentity_regulatedstatus_part2_duediligencequestionnaire", "true");
    ("no_luxsentity_regulatedstatus_part2_duediligencequestionnaire", "false");
  ] in
  let mapping = M.checkbox_mapping
    ~name:"sf_Account_SubscriptionInvestor_WLC_Publicly_Listed_On_A_Stock_Exchange_c"
    ~input_paths:[
      ("subdoc", ["subdoc"; "luxsentity_regulatedstatus_part2_duediligencequestionnaire"; "rawValue"]);
    ]
    ~output_path:["sf_Account_SubscriptionInvestor_WLC_Publicly_Listed_On_A_Stock_Exchange_c"; "rawValue"]
    ~option_map
  in
  let result = M.apply_mapping mapping input in
  let keys = JP.get_string_list result
    ["sf_Account_SubscriptionInvestor_WLC_Publicly_Listed_On_A_Stock_Exchange_c"; "rawValue"; "selectedKeys"]
  in
  Alcotest.(check (list string)) "mapped keys" ["true"] keys

let suite = [
  ("mappings", [
    Alcotest.test_case "textbox mapping" `Quick test_textbox_mapping;
    Alcotest.test_case "checkbox mapping" `Quick test_checkbox_mapping;
  ]);
]
