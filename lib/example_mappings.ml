module DT = Data_types
module ST = Source_table
module TT = Target_table

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

let string_value (st : DT.string_type option) : string =
  match st with
  | Some s -> s.value
  | None -> ""

let selected_keys (mct : DT.multiple_checkbox_type option) : string list =
  match mct with
  | Some mc -> mc.selected_keys
  | None -> []

let first_non_empty (values : string list) : string =
  match List.find_opt (fun s -> s <> "") values with
  | Some s -> s
  | None -> ""

let map_commitment (src : ST.source_table_fields_map) : DT.money_type =
  let raw = match src.lp_signatory with
    | Some lp ->
      (match lp.value_sub_fields with
       | Some fields -> string_value fields.asa_commitment_amount
       | None -> "")
    | None -> ""
  in
  let amount_str = String.split_on_char ',' raw |> String.concat "" in
  let amount_val = match float_of_string_opt amount_str with
    | Some f -> f
    | None -> 0.0
  in
  let amount = DT.make_number_type ~type_id:"Number" ~value:amount_val () in
  let sub_fields = DT.make_money_sub_fields ~amount () in
  DT.make_money_type ~type_id:"Money" ~value_sub_fields:sub_fields ()

let map_investor_name (src : ST.source_table_fields_map) : DT.string_type =
  let aml = string_value src.asa_fullname_investorname_amlquestionnaire in
  let general = string_value src.asa_fullname_investorname_generalinfo1 in
  let value = first_non_empty [aml; general] in
  DT.make_string_type ~type_id:"String" ~value ()

let map_regulated_status (src : ST.source_table_fields_map) : DT.radio_group_type =
  let option_map = [
    ("yes_luxsentity_regulatedstatus_part2_duediligencequestionnaire", "true");
    ("no_luxsentity_regulatedstatus_part2_duediligencequestionnaire", "false");
  ] in
  let keys = selected_keys src.luxsentity_regulatedstatus_part2_duediligencequestionnaire in
  let mapped = List.filter_map (fun k -> List.assoc_opt k option_map) keys in
  let selected_key = match mapped with
    | first :: _ -> first
    | [] -> ""
  in
  DT.make_radio_group_type ~type_id:"RadioGroup" ~selected_key ()

let map_international_supplements (src : ST.source_table_fields_map) : DT.multiple_checkbox_type =
  let option_map = [
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
  ] in
  let indi_keys = selected_keys src.indi_internationalsupplements_part1_duediligencequestionnaire in
  let entity_keys = selected_keys src.entity_internationalsupplements_part1_duediligencequestionnaire in
  let all_keys = indi_keys @ entity_keys in
  let mapped = List.filter_map (fun k -> List.assoc_opt k option_map) all_keys in
  let unique = List.fold_left
    (fun acc key -> if List.mem key acc then acc else key :: acc)
    [] mapped |> List.rev
  in
  DT.make_multiple_checkbox_type ~type_id:"MultipleCheckbox" ~selected_keys:unique ()

let map_signer_name (src : ST.source_table_fields_map)
    : DT.string_type * DT.string_type * DT.string_type =
  let fullname = match src.lp_signatory with
    | Some lp ->
      (match lp.value_sub_fields with
       | Some fields ->
         let ind = string_value fields.individual_subscribername_signaturepage in
         let ent = string_value fields.entity_authorizedname_signaturepage in
         first_non_empty [ind; ent]
       | None -> "")
    | None -> ""
  in
  let parts = split_name fullname in
  ( DT.make_string_type ~type_id:"String" ~value:parts.first_name (),
    DT.make_string_type ~type_id:"String" ~value:parts.middle_name (),
    DT.make_string_type ~type_id:"String" ~value:parts.last_name () )

let map_w9_tin_type (src : ST.source_table_fields_map) : DT.radio_group_type =
  match src.w9 with
  | Some w9_field ->
    (match w9_field.value_sub_fields with
     | Some fields ->
       let line2 = string_value fields.w9_line2 in
       if line2 <> "" then
         DT.make_radio_group_type ~type_id:"RadioGroup" ~selected_key:"" ()
       else
         let has_ssn = string_value fields.w9_parti_ssn1 <> "" in
         let key = if has_ssn then "SSN" else "EIN" in
         DT.make_radio_group_type ~type_id:"RadioGroup" ~selected_key:key ()
     | None ->
       DT.make_radio_group_type ~type_id:"RadioGroup" ~selected_key:"EIN" ())
  | None ->
    DT.make_radio_group_type ~type_id:"RadioGroup" ~selected_key:"EIN" ()

let transform (src : ST.source_table_fields_map) : TT.target_table_fields_map =
  let commitment = map_commitment src in
  let investor_name = map_investor_name src in
  let regulated_status = map_regulated_status src in
  let intl_supplements = map_international_supplements src in
  let (first, middle, last) = map_signer_name src in
  let tin_type = map_w9_tin_type src in
  TT.make_target_table_fields_map
    ~sf_agreement_null_commitment_c:commitment
    ~sf_account_subscription_investor_name:investor_name
    ~sf_account_subscription_investor_wlc_publicly_listed_on_a_stock_exchange_c:regulated_status
    ~sf_agreement_null_wlc_international_supplements_c:intl_supplements
    ~sf_agreement_null_signer_first_name:first
    ~sf_agreement_null_signer_middle_name:middle
    ~sf_agreement_null_signer_last_name:last
    ~sf_tax_form_w9_us_tin_type_c:tin_type
    ()
