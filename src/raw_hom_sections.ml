(**************************************************************************)
(*                                   Ott                                  *)
(*                                                                        *)
(*  Raw hom-section desugaring                                            *)
(*                                                                        *)
(*  Shared between import expansion and typechecking.                      *)
(**************************************************************************)

open Types

type extra_tbl = (string, (raw_bindspec list * raw_homomorphism list)) Hashtbl.t
type used_tbl = (string, unit) Hashtbl.t

let build_extra_table (hss : raw_hom_section list) : extra_tbl =
  let tbl : extra_tbl = Hashtbl.create 64 in
  List.iter (fun hs ->
    List.iter (fun hsi ->
      let key = hs.raw_hs_wrapper ^ hsi.raw_hsi_name in
      match Hashtbl.find_opt tbl key with
      | None -> Hashtbl.add tbl key (hsi.raw_hsi_bs, hsi.raw_hsi_homs)
      | Some (bs0, homs0) ->
        Hashtbl.replace tbl key (bs0 @ hsi.raw_hsi_bs, homs0 @ hsi.raw_hsi_homs)
    ) hs.raw_hs_hsis
  ) hss;
  tbl

let check_all_used ~(extras : extra_tbl) ~(used : used_tbl) : unit =
  let unused =
    Hashtbl.fold (fun k _ acc -> if Hashtbl.mem used k then acc else k :: acc) extras []
  in
  match unused with
  | [] -> ()
  | ks ->
    let ks = List.sort_uniq String.compare ks in
    failwith
      ("hom section contains items for nonexistent production or defn names: "
       ^ String.concat ", " ks)

let mark_used (used : used_tbl) (k : string) : unit =
  if not (Hashtbl.mem used k) then Hashtbl.add used k ()

let apply_to_prod ~(extras : extra_tbl) ~(used : used_tbl) ~(pn_wrapper : string) (rp : raw_prod)
  : raw_prod =
  let key = pn_wrapper ^ rp.raw_prod_name in
  match Hashtbl.find_opt extras key with
  | None -> rp
  | Some (bs, homs) ->
    mark_used used key;
    { rp with raw_prod_homs = rp.raw_prod_homs @ homs; raw_prod_bs = rp.raw_prod_bs @ bs }

let apply_to_rule ~(extras : extra_tbl) ~(used : used_tbl) (rr : raw_rule) : raw_rule =
  { rr with
    raw_rule_ps =
      List.map (apply_to_prod ~extras ~used ~pn_wrapper:rr.raw_rule_pn_wrapper) rr.raw_rule_ps
  }

let apply_to_defn ~(extras : extra_tbl) ~(used : used_tbl) ~(dc_wrapper : string) (rd : raw_defn)
  : raw_defn =
  let key = dc_wrapper ^ rd.raw_d_name in
  match Hashtbl.find_opt extras key with
  | None -> rd
  | Some (bs, homs) ->
    if bs <> [] then failwith "cannot have a bindspec for a defn (in a hom section)";
    mark_used used key;
    { rd with raw_d_homs = rd.raw_d_homs @ homs }

let apply_to_fundefn ~(extras : extra_tbl) ~(used : used_tbl) (rfd : raw_fundefn) : raw_fundefn =
  let key = rfd.raw_fd_name in
  match Hashtbl.find_opt extras key with
  | None -> rfd
  | Some (bs, homs) ->
    if bs <> [] then failwith "cannot have a bindspec for a fundefn (in a hom section)";
    mark_used used key;
    { rfd with raw_fd_homs = rfd.raw_fd_homs @ homs }

let apply_to_fun_or_reln_defnclass ~(extras : extra_tbl) ~(used : used_tbl)
    (dc : raw_fun_or_reln_defnclass) : raw_fun_or_reln_defnclass =
  match dc with
  | Raw_RDC rdc ->
    Raw_RDC { rdc with
      raw_dc_defns =
        List.map
          (apply_to_defn ~extras ~used ~dc_wrapper:rdc.raw_dc_wrapper)
          rdc.raw_dc_defns;
    }
  | Raw_FDC rfdc ->
    Raw_FDC { rfdc with
      raw_fdc_fundefns = List.map (apply_to_fundefn ~extras ~used) rfdc.raw_fdc_fundefns;
    }

let apply_to_raw_syntaxdefn (rsd : raw_syntaxdefn) : raw_syntaxdefn =
  if rsd.raw_sd_hss = [] then rsd
  else
    let extras = build_extra_table rsd.raw_sd_hss in
    let used : used_tbl = Hashtbl.create 64 in
    let raw_sd_rs = List.map (apply_to_rule ~extras ~used) rsd.raw_sd_rs in
    let raw_sd_dcs = List.map (apply_to_fun_or_reln_defnclass ~extras ~used) rsd.raw_sd_dcs in
    check_all_used ~extras ~used;
    { rsd with
      raw_sd_rs;
      raw_sd_dcs;
      raw_sd_hss = [];
    }

let apply_to_items (items : raw_item list) : raw_item list =
  let hss =
    List.filter_map (function Raw_item_hs hs -> Some hs | _ -> None) items
  in
  if hss = [] then items
  else
    let extras = build_extra_table hss in
    let used : used_tbl = Hashtbl.create 64 in
    let items' =
      List.filter_map (fun ri ->
        match ri with
        | Raw_item_hs _ -> None
        | Raw_item_rs raw_rs -> Some (Raw_item_rs (List.map (apply_to_rule ~extras ~used) raw_rs))
        | Raw_item_dcs dc -> Some (Raw_item_dcs (apply_to_fun_or_reln_defnclass ~extras ~used dc))
        | _ -> Some ri
      ) items
    in
    check_all_used ~extras ~used;
    items'
