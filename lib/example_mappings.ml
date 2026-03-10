module M = Mappings
module JP = Json_path

type name_parts = {
  first_name : string;
  middle_name : string;
  last_name : string;
}

let split_name (fullname : string) : name_parts =
  let trimmed = String.trim fullname in
  if trimmed = "" then
    { first_name = ""; middle_name = ""; last_name = "" }
  else
    let parts = String.split_on_char ' ' trimmed
                |> List.filter (fun s -> s <> "") in
    match parts with
    | [] -> { first_name = ""; middle_name = ""; last_name = "" }
    | [single] -> { first_name = single; middle_name = ""; last_name = single }
    | [first; last] -> { first_name = first; middle_name = ""; last_name = last }
    | first :: rest ->
      let last = List.nth rest (List.length rest - 1) in
      let middle = List.filteri (fun i _ -> i < List.length rest - 1) rest
                   |> String.concat " " in
      { first_name = first; middle_name = middle; last_name = last }

(* === TEXTBOX MAPPINGS === *)

let commitment_mapping () =
  M.textbox_mapping
    ~name:"sf_Agreement_null_Commitment_c"
    ~input_paths:[
      ("subdoc", ["subdoc"; "lp_signatory"; "asa_commitment_amount"]);
    ]
    ~output_path:["sf_Agreement_null_Commitment_c"]

let investor_name_mapping () =
  M.textbox_mapping
    ~name:"sf_Account_SubscriptionInvestor_Name"
    ~input_paths:[
      ("subdoc_aml", ["subdoc"; "asa_fullname_investorname_amlquestionnaire"]);
      ("subdoc_general", ["subdoc"; "asa_fullname_investorname_generalinfo1"]);
    ]
    ~output_path:["sf_Account_SubscriptionInvestor_Name"]

(* === CHECKBOX MAPPINGS === *)

let regulated_status_mapping () =
  M.checkbox_mapping
    ~name:"sf_Account_SubscriptionInvestor_WLC_Publicly_Listed_On_A_Stock_Exchange_c"
    ~input_paths:[
      ("subdoc", ["subdoc"; "luxsentity_regulatedstatus_part2_duediligencequestionnaire"]);
    ]
    ~output_path:["sf_Account_SubscriptionInvestor_WLC_Publicly_Listed_On_A_Stock_Exchange_c"]
    ~option_map:[
      ("yes_luxsentity_regulatedstatus_part2_duediligencequestionnaire", "true");
      ("no_luxsentity_regulatedstatus_part2_duediligencequestionnaire", "false");
    ]

let international_supplements_mapping () =
  M.checkbox_mapping
    ~name:"sf_Agreement_null_WLC_International_Supplements_c"
    ~input_paths:[
      ("subdoc_indi", ["subdoc"; "indi_internationalsupplements_part1_duediligencequestionnaire"]);
      ("subdoc_entity", ["subdoc"; "entity_internationalsupplements_part1_duediligencequestionnaire"]);
    ]
    ~output_path:["sf_Agreement_null_WLC_International_Supplements_c"]
    ~option_map:[
      ("eea_indi_internationalsupplements_part1_duediligencequestionnaire", "European Economic Area - Supplement");
      ("uk_indi_internationalsupplements_part1_duediligencequestionnaire", "United Kingdom - Supplement");
      ("swiss_indi_internationalsupplements_part1_duediligencequestionnaire", "Swiss - Supplement");
      ("canada_indi_internationalsupplements_part1_duediligencequestionnaire", "Canadian - Supplement");
      ("japan_indi_internationalsupplements_part1_duediligencequestionnaire", "Japanese - Supplement");
      ("none_indi_internationalsupplements_part1_duediligencequestionnaire", "No Supplement");
      ("eea_entity_internationalsupplements_part1_duediligencequestionnaire", "European Economic Area - Supplement");
      ("uk_entity_internationalsupplements_part1_duediligencequestionnaire", "United Kingdom - Supplement");
      ("swiss_entity_internationalsupplements_part1_duediligencequestionnaire", "Swiss - Supplement");
      ("canada_entity_internationalsupplements_part1_duediligencequestionnaire", "Canadian - Supplement");
      ("japan_entity_internationalsupplements_part1_duediligencequestionnaire", "Japanese - Supplement");
      ("none_entity_internationalsupplements_part1_duediligencequestionnaire", "No Supplement");
    ]

(* === CUSTOM MAPPINGS === *)

let signer_name_mapping () =
  M.custom_mapping
    ~name:"sf_Agreement_null_Signer_Name"
    ~input_paths:[
      ("subdoc_ind", ["subdoc"; "lp_signatory"; "individual_subscribername_signaturepage"]);
      ("subdoc_ent", ["subdoc"; "lp_signatory"; "entity_authorizedname_signaturepage"]);
    ]
    ~output_path:[]
    ~transform_fn:(fun input ->
      let values =
        match input with
        | `Assoc fields ->
          List.filter_map
            (fun (_alias, v) ->
              match JP.get_path v ["value"] with
              | Some (`String s) when s <> "" -> Some s
              | _ -> None)
            fields
        | _ -> []
      in
      let fullname = String.concat "" values |> String.trim in
      let parts = split_name fullname in
      `Assoc [
        ("sf_Agreement_null_Signer_FirstName",
         `Assoc [("value", `String parts.first_name)]);
        ("sf_Agreement_null_Signer_MiddleName",
         `Assoc [("value", `String parts.middle_name)]);
        ("sf_Agreement_null_Signer_LastName",
         `Assoc [("value", `String parts.last_name)]);
      ])

let w9_tin_type_mapping () =
  M.custom_mapping
    ~name:"sf_TaxForm_W9_US_TIN_Type_c"
    ~input_paths:[
      ("w9_ssn", ["subdoc"; "w9"; "w9_parti_ssn1"]);
      ("w9_ein", ["subdoc"; "w9"; "w9_parti_ein1"]);
      ("w9_line2", ["subdoc"; "w9"; "w9_line2"]);
    ]
    ~output_path:["sf_TaxForm_W9_US_TIN_Type_c"]
    ~transform_fn:(fun input ->
      let w9_line2_value =
        match input with
        | `Assoc fields ->
          (match List.assoc_opt "w9_line2" fields with
           | Some v -> JP.get_string_or_empty v ["value"]
           | None -> "")
        | _ -> ""
      in
      if w9_line2_value = "" then
        let has_ssn =
          match input with
          | `Assoc fields ->
            (match List.assoc_opt "w9_ssn" fields with
             | Some v -> JP.get_string_or_empty v ["value"] <> ""
             | None -> false)
          | _ -> false
        in
        `Assoc [("value", `String (if has_ssn then "SSN" else "EIN"))]
      else
        `Assoc [("value", `String "")])

let all_mappings () = [
  commitment_mapping ();
  investor_name_mapping ();
  regulated_status_mapping ();
  international_supplements_mapping ();
  signer_name_mapping ();
  w9_tin_type_mapping ();
]
