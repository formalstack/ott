(**************************************************************************)
(*                                   Ott                                  *)
(*                                                                        *)
(*  Import system — module loading, dependency analysis, flattening       *)
(**************************************************************************)

open Types

module StringSet = Set.Make(String)

(* ------------------------------------------------------------------ *)
(* Internal resolver state                                             *)
(* ------------------------------------------------------------------ *)

type export_kind =
  | Export_syntax
  | Export_defnclass
  | Export_fundefnclass
  | Export_defn
  | Export_fundefn
  | Export_subst
  | Export_freevar

type resolved_import_item = {
  raw_item : raw_import_item;
  resolved_name : string;
  export_kind : export_kind;
}

type module_index = {
  canonical_path : string;
  expanded_items : raw_item list;
  module_scope_info : scope_info;
  nested_bindings : binding_info list;
  nested_import_sites : import_site_info list;
  synonym_map : (string * string) list;
  syntax_names : StringSet.t;
  syntax_primaries : StringSet.t;
  other_known_names : StringSet.t;
  export_kinds_by_name : (string, export_kind list) Hashtbl.t;
  binding_kinds_by_name : (string, import_binding_kind list) Hashtbl.t;
  class_members : (string, string list) Hashtbl.t;
  member_parent_class : (string, string) Hashtbl.t;
  dep_map : (string, StringSet.t) Hashtbl.t;
  name_sources : (string, StringSet.t) Hashtbl.t;
  indexvar_names : StringSet.t;
}

type resolver_state = {
  loaded_modules : (string, module_index) Hashtbl.t;
  file_of_module : (string, string) Hashtbl.t;
  mutable imported_file_list : string list;
  mutable visiting : StringSet.t;
}

type resolved_import = {
  injected_items : raw_item list;
  site_info : import_site_info;
  scope_names : StringSet.t;
  allowed_judgement_roots : StringSet.t;
  transitive_only_names : StringSet.t;
}

type validated_import_items = {
  resolved_items : resolved_import_item list;
  seeds : StringSet.t;
}

type explicit_import_view = {
  rename_map : (string * string) list;
  root_links : import_root_link list;
  scope_names : StringSet.t;
  allowed_judgement_roots : StringSet.t;
  bindings : binding_info list;
}

type internalized_import_items = {
  items : raw_item list;
  transitive_only_names : StringSet.t;
  bindings : binding_info list;
}

type resolved_source_file = {
  source_file : string;
  imported_items : raw_item list;
  local_items : raw_item list;
  scope_info : scope_info;
  direct_sites : import_site_info list;
}

type decl_kind =
  | Decl_rule
  | Decl_metavar
  | Decl_defnclass
  | Decl_fundefnclass
  | Decl_subst
  | Decl_freevar
  | Decl_subrule
  | Decl_contextrule
  | Decl_parsing

type dedup_policy =
  | Dedup_never
  | Dedup_if_any_known
  | Dedup_if_all_known

type decl_group = {
  decl_id : string;
  loc : loc;
  bind_names : string list;
  source_names : string list;
  dedup_names : string list;
  dedup_policy : dedup_policy;
  allow_parallel_bindings : bool;
}

let decl_kind_tag = function
  | Decl_rule -> "rule"
  | Decl_metavar -> "metavar"
  | Decl_defnclass -> "defnclass"
  | Decl_fundefnclass -> "fundefnclass"
  | Decl_subst -> "subst"
  | Decl_freevar -> "freevar"
  | Decl_subrule -> "subrule"
  | Decl_contextrule -> "contextrule"
  | Decl_parsing -> "parsing"

let decl_id_of_loc ~(kind : decl_kind) ~(loc : loc) ~(name : string) : string =
  let kind = decl_kind_tag kind in
  match loc with
  | [] -> "<unknown>:" ^ kind ^ ":" ^ name
  | loc1 :: _ ->
    let p = loc1.Location.loc_start in
    let col = p.Lexing.pos_cnum - p.Lexing.pos_bol in
    p.Lexing.pos_fname ^ ":" ^ string_of_int p.Lexing.pos_lnum ^ ":"
    ^ string_of_int col ^ ":" ^ kind ^ ":" ^ name

let make_decl_group ~kind ~loc ~key_name ~bind_names ~source_names ~dedup_names
    ~dedup_policy ~allow_parallel_bindings : decl_group =
  {
    decl_id = decl_id_of_loc ~kind ~loc ~name:key_name;
    loc;
    bind_names;
    source_names;
    dedup_names;
    dedup_policy;
    allow_parallel_bindings;
  }

let rule_decl_group (rr : raw_rule) : decl_group =
  let names = List.map fst rr.raw_rule_ntr_names in
  make_decl_group
    ~kind:Decl_rule
    ~loc:rr.raw_rule_loc
    ~key_name:rr.raw_rule_ntr_name
    ~bind_names:names
    ~source_names:names
    ~dedup_names:[rr.raw_rule_ntr_name]
    ~dedup_policy:Dedup_if_any_known
    ~allow_parallel_bindings:(Import_naming.is_synthesized_root rr.raw_rule_ntr_name)

let metavar_decl_group (raw_md : raw_metavardefn) : decl_group =
  let names = List.map fst raw_md.raw_mvd_names in
  make_decl_group
    ~kind:Decl_metavar
    ~loc:raw_md.raw_mvd_loc
    ~key_name:raw_md.raw_mvd_name
    ~bind_names:names
    ~source_names:names
    ~dedup_names:[raw_md.raw_mvd_name]
    ~dedup_policy:Dedup_if_any_known
    ~allow_parallel_bindings:false

let reln_defnclass_decl_group (rdc : raw_defnclass) : decl_group =
  let members = List.map (fun rd -> rd.raw_d_name) rdc.raw_dc_defns in
  make_decl_group
    ~kind:Decl_defnclass
    ~loc:rdc.raw_dc_loc
    ~key_name:rdc.raw_dc_name
    ~bind_names:[rdc.raw_dc_name]
    ~source_names:(rdc.raw_dc_name :: members)
    ~dedup_names:[rdc.raw_dc_name]
    ~dedup_policy:Dedup_if_any_known
    ~allow_parallel_bindings:false

let fundefnclass_decl_group (rfdc : raw_fundefnclass) : decl_group =
  let members = List.map (fun rfd -> rfd.raw_fd_name) rfdc.raw_fdc_fundefns in
  make_decl_group
    ~kind:Decl_fundefnclass
    ~loc:rfdc.raw_fdc_loc
    ~key_name:rfdc.raw_fdc_name
    ~bind_names:[rfdc.raw_fdc_name]
    ~source_names:(rfdc.raw_fdc_name :: members)
    ~dedup_names:[rfdc.raw_fdc_name]
    ~dedup_policy:Dedup_if_any_known
    ~allow_parallel_bindings:false

let subst_decl_group (sb : raw_subst) : decl_group =
  make_decl_group
    ~kind:Decl_subst
    ~loc:sb.raw_sb_loc
    ~key_name:sb.raw_sb_name
    ~bind_names:[sb.raw_sb_name]
    ~source_names:[sb.raw_sb_name]
    ~dedup_names:[]
    ~dedup_policy:Dedup_never
    ~allow_parallel_bindings:false

let freevar_decl_group (fv : raw_freevar) : decl_group =
  make_decl_group
    ~kind:Decl_freevar
    ~loc:fv.raw_fv_loc
    ~key_name:fv.raw_fv_name
    ~bind_names:[fv.raw_fv_name]
    ~source_names:[fv.raw_fv_name]
    ~dedup_names:[]
    ~dedup_policy:Dedup_never
    ~allow_parallel_bindings:false

let subrule_decl_group (sr : raw_subrule) : decl_group =
  make_decl_group
    ~kind:Decl_subrule
    ~loc:sr.raw_sr_loc
    ~key_name:(sr.raw_sr_lower ^ "<::" ^ sr.raw_sr_upper)
    ~bind_names:[]
    ~source_names:[]
    ~dedup_names:[sr.raw_sr_lower; sr.raw_sr_upper]
    ~dedup_policy:Dedup_if_all_known
    ~allow_parallel_bindings:false

let contextrule_decl_group (cr : raw_contextrule) : decl_group =
  make_decl_group
    ~kind:Decl_contextrule
    ~loc:cr.raw_cr_loc
    ~key_name:(cr.raw_cr_ntr ^ ":" ^ cr.raw_cr_target ^ ":" ^ cr.raw_cr_hole)
    ~bind_names:[]
    ~source_names:[]
    ~dedup_names:[]
    ~dedup_policy:Dedup_never
    ~allow_parallel_bindings:false

let parsing_decl_group (pa : raw_parsing_annotations) : decl_group =
  make_decl_group
    ~kind:Decl_parsing
    ~loc:pa.raw_pa_loc
    ~key_name:"parsing"
    ~bind_names:[]
    ~source_names:[]
    ~dedup_names:[]
    ~dedup_policy:Dedup_never
    ~allow_parallel_bindings:false

let decl_groups_of_item (ri : raw_item) : decl_group list =
  match ri with
  | Raw_item_md raw_md -> [metavar_decl_group raw_md]
  | Raw_item_rs raw_rs -> List.map rule_decl_group raw_rs
  | Raw_item_dcs (Raw_RDC rdc) -> [reln_defnclass_decl_group rdc]
  | Raw_item_dcs (Raw_FDC rfdc) -> [fundefnclass_decl_group rfdc]
  | Raw_item_srs raw_srs -> List.map subrule_decl_group raw_srs
  | Raw_item_crs raw_crs -> List.map contextrule_decl_group raw_crs
  | Raw_item_sbs raw_sbs -> List.map subst_decl_group raw_sbs
  | Raw_item_fvs raw_fvs -> List.map freevar_decl_group raw_fvs
  | Raw_item_pas raw_pas -> List.map parsing_decl_group raw_pas
  | _ -> []

let decl_groups_of_items (items : raw_item list) : decl_group list =
  List.concat_map decl_groups_of_item items

let declared_names_of_decl_groups (decls : decl_group list) : StringSet.t =
  StringSet.of_list
    (List.concat_map (fun decl -> decl.source_names) decls)

let declared_names_of_item (ri : raw_item) : StringSet.t =
  declared_names_of_decl_groups (decl_groups_of_item ri)

let declared_names_of_items (items : raw_item list) : StringSet.t =
  declared_names_of_decl_groups (decl_groups_of_items items)

let dedup_seed_names_of_items (items : raw_item list) : StringSet.t =
  StringSet.of_list
    (List.concat_map (fun decl ->
      match decl.dedup_policy with
      | Dedup_if_any_known -> decl.bind_names
      | Dedup_if_all_known | Dedup_never -> []
    ) (decl_groups_of_items items))

let should_suppress_decl_by_known_names ~(known_names : StringSet.t)
    (decl : decl_group) : bool =
  match decl.dedup_policy with
  | Dedup_never -> false
  | Dedup_if_any_known ->
    List.exists (fun name -> StringSet.mem name known_names) decl.dedup_names
  | Dedup_if_all_known ->
    decl.dedup_names <> []
    && List.for_all (fun name -> StringSet.mem name known_names) decl.dedup_names

let create_resolver_state () = {
  loaded_modules = Hashtbl.create 16;
  file_of_module = Hashtbl.create 16;
  imported_file_list = [];
  visiting = StringSet.empty;
}

let active_resolver_state : resolver_state option ref = ref None

let activate_fresh_resolver_state () =
  let state = create_resolver_state () in
  active_resolver_state := Some state;
  state

let imported_files () =
  match !active_resolver_state with
  | Some state -> List.rev state.imported_file_list
  | None -> []

let get_loaded_module_items (canonical_path : string) : raw_item list option =
  match !active_resolver_state with
  | Some state ->
    (match Hashtbl.find_opt state.loaded_modules canonical_path with
     | Some index -> Some index.expanded_items
     | None -> None)
  | None -> None

let remember_imported_file (state : resolver_state) (canonical_path : string) =
  if not (List.mem canonical_path state.imported_file_list) then
    state.imported_file_list <- canonical_path :: state.imported_file_list

let claim_module_alias_path ~state ~loc ~module_name ~canonical_path =
  (match Hashtbl.find_opt state.file_of_module module_name with
   | Some existing_path when existing_path <> canonical_path ->
     Auxl.error (Some loc)
       ("Imported module '" ^ module_name
        ^ "' resolves to multiple files:\n  "
        ^ existing_path ^ "\n  " ^ canonical_path ^ "\n")
   | _ -> ());
  Hashtbl.replace state.file_of_module module_name canonical_path

let is_imported_loc ic loc =
  match ic.ic_imported_files with
  | [] -> false
  | files ->
    match loc with
    | [] -> false
    | l :: _ -> List.mem l.Location.loc_start.Lexing.pos_fname files

let is_imported_file ic filename =
  List.mem filename ic.ic_imported_files

let lookup_fallback_binding ic name =
  lookup_import_binding ic name

let lookup_scope_info ic source_file =
  Hashtbl.find_opt ic.ic_scope_info_by_source_file source_file

let make_provider_info ~module_name ~canonical_path ~homs ~loc : provider_info =
  {
    pi_module = module_name;
    pi_file = canonical_path;
    pi_homs = homs;
    pi_default_id = String.capitalize_ascii module_name;
    pi_loc = loc;
  }

let direct_providers ic =
  ic.ic_direct_providers

let sort_homs (homs : (string * string) list) : (string * string) list =
  List.sort compare homs

let provider_exact_key (provider : provider_info)
  : string * string * string * (string * string) list =
  ( provider.pi_module,
    provider.pi_file,
    provider.pi_default_id,
    sort_homs provider.pi_homs )

let provider_equiv (p1 : provider_info) (p2 : provider_info) : bool =
  provider_exact_key p1 = provider_exact_key p2

let dedup_providers_exact (providers : provider_info list) : provider_info list =
  let seen :
      ((string * string * string * (string * string) list), unit)
      Hashtbl.t =
    Hashtbl.create (List.length providers)
  in
  let acc_rev = ref [] in
  List.iter (fun provider ->
    let key = provider_exact_key provider in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.replace seen key ();
      acc_rev := provider :: !acc_rev
    end
  ) providers;
  List.rev !acc_rev

let binding_info_equiv (b1 : binding_info) (b2 : binding_info) : bool =
  b1.bi_local_name = b2.bi_local_name
  && b1.bi_origin_name = b2.bi_origin_name
  && provider_equiv b1.bi_provider b2.bi_provider
  && b1.bi_visibility = b2.bi_visibility
  && List.sort compare b1.bi_kinds = List.sort compare b2.bi_kinds

let add_unique_binding
    (table : (string, binding_info) Hashtbl.t)
    (ambiguous : (string, unit) Hashtbl.t)
    ~(key : string)
    ~(value : binding_info)
  : unit =
  if Hashtbl.mem ambiguous key then ()
  else
    match Hashtbl.find_opt table key with
    | None -> Hashtbl.replace table key value
    | Some existing when binding_info_equiv existing value -> ()
    | Some _ ->
      Hashtbl.remove table key;
      Hashtbl.replace ambiguous key ()

let import_context_of_scope_infos
    ~(direct_providers : provider_info list)
    ~(imported_files : string list)
    (scope_info_by_file : (string, scope_info) Hashtbl.t)
  : import_context =
  let binding_info_by_local_name : (string, binding_info) Hashtbl.t =
    Hashtbl.create 64
  in
  let ambiguous_bindings : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let add_binding (binding : binding_info) =
    add_unique_binding
      binding_info_by_local_name
      ambiguous_bindings
      ~key:binding.bi_local_name
      ~value:binding
  in
  Hashtbl.iter (fun _source_file scope ->
    List.iter (fun site ->
      List.iter add_binding site.isi_bindings
    ) scope.si_direct_imports
  ) scope_info_by_file;
  {
    ic_direct_providers = direct_providers;
    ic_imported_files = imported_files;
    ic_binding_info_by_local_name = binding_info_by_local_name;
    ic_scope_info_by_source_file = scope_info_by_file;
  }

let dedup_bindings (bindings : binding_info list) : binding_info list =
  let by_local_name : (string, binding_info) Hashtbl.t =
    Hashtbl.create (List.length bindings)
  in
  let acc_rev = ref [] in
  List.iter (fun binding ->
    match Hashtbl.find_opt by_local_name binding.bi_local_name with
    | None ->
      Hashtbl.replace by_local_name binding.bi_local_name binding;
      acc_rev := binding :: !acc_rev
    | Some existing ->
      if binding_info_equiv existing binding then ()
      else
        Auxl.error None
          ("Internal error: inconsistent import binding for name '"
           ^ binding.bi_local_name ^ "'.\n")
  ) bindings;
  List.rev !acc_rev

let current_output_source_file : string option ref = ref None

let with_current_source_file (source_file : string option) (f : unit -> 'a) : 'a =
  let saved = !current_output_source_file in
  current_output_source_file := source_file;
  try
    let result = f () in
    current_output_source_file := saved;
    result
  with exn ->
    current_output_source_file := saved;
    raise exn

let find_binding_in_sites (sites : import_site_info list) (name : string)
  : binding_info option =
  List.find_map (fun site ->
    List.find_opt (fun binding -> binding.bi_local_name = name) site.isi_bindings
  ) sites

let find_provider_in_sites (sites : import_site_info list)
    (matches : provider_info -> bool)
  : provider_info option =
  List.find_map (fun site ->
    if matches site.isi_provider then
      Some site.isi_provider
    else
      List.find_map (fun binding ->
        if matches binding.bi_provider then Some binding.bi_provider else None
      ) site.isi_bindings
  ) sites

let lookup_unique_provider (ic : import_context) (matches : provider_info -> bool)
  : provider_info option =
  let found = ref None in
  let ambiguous = ref false in
  let remember provider =
    if matches provider then
      match !found with
      | None -> found := Some provider
      | Some existing when provider_equiv existing provider -> ()
      | Some _ -> ambiguous := true
  in
  List.iter remember ic.ic_direct_providers;
  Hashtbl.iter (fun _source_file scope ->
    List.iter (fun site ->
      remember site.isi_provider;
      List.iter (fun binding -> remember binding.bi_provider) site.isi_bindings
    ) scope.si_direct_imports
  ) ic.ic_scope_info_by_source_file;
  if !ambiguous then None else !found

let lookup_binding_in_source ic ~source_file name =
  match lookup_scope_info ic source_file with
  | Some scope ->
    (match find_binding_in_sites scope.si_direct_imports name with
     | Some binding -> Some binding
     | None -> lookup_fallback_binding ic name)
  | None -> lookup_fallback_binding ic name

let lookup_binding_for_current_source ic name =
  match !current_output_source_file with
  | Some source_file -> lookup_binding_in_source ic ~source_file name
  | None -> lookup_fallback_binding ic name

let lookup_provider_in_source ic ~source_file module_name =
  match lookup_scope_info ic source_file with
  | Some scope ->
    (match find_provider_in_sites scope.si_direct_imports
             (fun provider -> provider.pi_module = module_name) with
     | Some provider -> Some provider
     | None -> lookup_unique_provider ic
                 (fun provider -> provider.pi_module = module_name))
  | None -> lookup_unique_provider ic
              (fun provider -> provider.pi_module = module_name)

let lookup_provider_for_current_source ic module_name =
  match !current_output_source_file with
  | Some source_file -> lookup_provider_in_source ic ~source_file module_name
  | None -> lookup_unique_provider ic
              (fun provider -> provider.pi_module = module_name)

let lookup_provider_by_file_in_source ic ~source_file canonical_path =
  match lookup_scope_info ic source_file with
  | Some scope ->
    (match find_provider_in_sites scope.si_direct_imports
             (fun provider -> provider.pi_file = canonical_path) with
     | Some provider -> Some provider
     | None -> lookup_unique_provider ic
                 (fun provider -> provider.pi_file = canonical_path))
  | None -> lookup_unique_provider ic
              (fun provider -> provider.pi_file = canonical_path)

let direct_providers_for_source_files ic (source_files : string list) : provider_info list =
  if source_files = [] then
    direct_providers ic
  else
    let providers =
      List.concat_map (fun source_file ->
        match lookup_scope_info ic source_file with
        | None -> []
        | Some scope ->
          List.map (fun site -> site.isi_provider) scope.si_direct_imports
      ) source_files
    in
    dedup_providers_exact providers

let root_links_in_source ic ~source_file ~provider_module : import_root_link list =
  match lookup_scope_info ic source_file with
  | None -> []
  | Some scope ->
    let seen : ((string * string * string * string), unit) Hashtbl.t = Hashtbl.create 16 in
    let acc_rev = ref [] in
    List.iter (fun site ->
      if site.isi_provider.pi_module = provider_module then
        List.iter (fun (link : import_root_link) ->
          let key =
            ( link.irl_provider_root,
              link.irl_provider_primary,
              link.irl_local_root,
              link.irl_local_primary )
          in
          if not (Hashtbl.mem seen key) then begin
            Hashtbl.replace seen key ();
            acc_rev := link :: !acc_rev
          end
        ) site.isi_root_links
    ) scope.si_direct_imports;
    List.rev !acc_rev

let display_name ic name =
  match lookup_binding_for_current_source ic name with
  | Some binding -> binding.bi_origin_name
  | None ->
    (match Import_naming.surface_of_internal_token name with
     | Some s -> s
     | None -> name)

(* ------------------------------------------------------------------ *)
(* File resolution                                                     *)
(* ------------------------------------------------------------------ *)

let canonical_existing_path ?loc (path : string) : string =
  try
    Unix.realpath path
  with Unix.Unix_error (err, _, _) ->
    Auxl.error loc
      ("Cannot canonicalize path '" ^ path ^ "': "
       ^ Unix.error_message err ^ "\n")

let resolve_module_file ~base_dir ~search_paths ~loc ~module_name requested_path =
  let candidates =
    if Filename.is_relative requested_path then
      (Filename.concat base_dir requested_path)
      :: List.map (fun dir -> Filename.concat dir requested_path) search_paths
    else
      [requested_path]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> canonical_existing_path ~loc path
  | None ->
    Auxl.error (Some loc)
      ("Cannot find imported module '" ^ module_name
       ^ "' at path '" ^ requested_path
       ^ "'. Searched:\n  " ^ String.concat "\n  " candidates ^ "\n")

(* ------------------------------------------------------------------ *)
(* Raw-level name extraction                                           *)
(* ------------------------------------------------------------------ *)

let ntrs_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_rs raw_rs ->
      List.concat_map (fun rr ->
        List.map fst rr.raw_rule_ntr_names
      ) raw_rs
    | _ -> []
  ) items

let ntr_primaries_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_rs raw_rs -> List.map (fun rr -> rr.raw_rule_ntr_name) raw_rs
    | _ -> []
  ) items

let mvrs_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_md raw_md ->
      List.map fst raw_md.raw_mvd_names
    | _ -> []
  ) items

let mvr_primaries_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_md raw_md -> [raw_md.raw_mvd_name]
    | _ -> []
  ) items

let dcs_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_dcs (Raw_RDC rdc) -> [rdc.raw_dc_name]
    | Raw_item_dcs (Raw_FDC rfdc) -> [rfdc.raw_fdc_name]
    | _ -> []
  ) items

let subst_names_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_sbs raw_sbs -> List.map (fun sb -> sb.raw_sb_name) raw_sbs
    | _ -> []
  ) items

let freevar_names_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_fvs raw_fvs -> List.map (fun fv -> fv.raw_fv_name) raw_fvs
    | _ -> []
  ) items

let defn_idents_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_dcs (Raw_RDC rdc) -> List.map (fun rd -> rd.raw_d_name) rdc.raw_dc_defns
    | _ -> []
  ) items

let fundefn_idents_of_items (items : raw_item list) : string list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_dcs (Raw_FDC rfdc) -> List.map (fun rfd -> rfd.raw_fd_name) rfdc.raw_fdc_fundefns
    | _ -> []
  ) items

let subrule_pairs_of_items (items : raw_item list) : (string * string) list =
  List.concat_map (fun ri ->
    match ri with
    | Raw_item_srs raw_srs ->
      List.map (fun sr -> (sr.raw_sr_lower, sr.raw_sr_upper)) raw_srs
    | _ -> []
  ) items

(* ------------------------------------------------------------------ *)
(* Synonym map: all names -> primary name                              *)
(* ------------------------------------------------------------------ *)

let build_synonym_map (items : raw_item list) : (string * string) list =
  let from_rules =
    List.concat_map (fun ri ->
      match ri with
      | Raw_item_rs raw_rs ->
        List.concat_map (fun rr ->
          let primary = rr.raw_rule_ntr_name in
          List.map (fun (name, _) -> (name, primary)) rr.raw_rule_ntr_names
        ) raw_rs
      | _ -> []
    ) items
  in
  let from_mvds =
    List.concat_map (fun ri ->
      match ri with
      | Raw_item_md raw_md ->
        let primary = raw_md.raw_mvd_name in
        List.map (fun (name, _) -> (name, primary)) raw_md.raw_mvd_names
      | _ -> []
    ) items
  in
  from_rules @ from_mvds

let resolve_to_primary synonym_map name =
  match List.assoc_opt name synonym_map with
  | Some p -> p
  | None -> name

(* ------------------------------------------------------------------ *)
(* Raw-level dependency analysis                                       *)
(* ------------------------------------------------------------------ *)

let idents_of_raw_ident ((_, s) : raw_ident) : string list = [s]

let rec idents_of_raw_comp_bound (rcb : raw_comp_bound) : string list =
  match rcb with
  | Raw_bound_comp (_, ri) -> idents_of_raw_ident ri
  | Raw_bound_comp_u (_, ri1, ri2) -> idents_of_raw_ident ri1 @ idents_of_raw_ident ri2
  | Raw_bound_comp_lu (_, ri1, ri2, _n, ri3) ->
    idents_of_raw_ident ri1 @ idents_of_raw_ident ri2 @ idents_of_raw_ident ri3

(* Extract identifier strings from raw elements (including listform bounds). *)
let rec idents_of_raw_element (e : raw_element) : string list =
  match e with
  | Raw_ident (_, ri) -> idents_of_raw_ident ri
  | Raw_option (_, es) | Raw_list (_, es) | Raw_nelist (_, es) ->
    List.concat_map idents_of_raw_element es
  | Raw_comp (_, es, rcb, ident_opt) ->
    let from_es = List.concat_map idents_of_raw_element es in
    let from_bound = idents_of_raw_comp_bound rcb in
    let from_ident =
      match ident_opt with
      | Some ri -> idents_of_raw_ident ri
      | None -> []
    in
    from_es @ from_bound @ from_ident
  | Raw_sugaroption (_, e) -> idents_of_raw_element e
  | Raw_dots _ -> []

let token_idents_in_string = Auxl.ott_ident_tokens_in_string

let rec idents_of_raw_mse (mse : raw_mse) : string list =
  match mse with
  | Raw_MVorNTExp (_, ri) -> idents_of_raw_ident ri
  | Raw_MVorNTListExp_dotform (_, ri1, _n, ri2) ->
    idents_of_raw_ident ri1 @ idents_of_raw_ident ri2
  | Raw_MVorNTListExp_comp (_, ri, rcb) ->
    idents_of_raw_ident ri @ idents_of_raw_comp_bound rcb
  | Raw_Aux (_, ri1, ri2) ->
    idents_of_raw_ident ri1 @ idents_of_raw_ident ri2
  | Raw_AuxList_dotform (_, ri1, ri2, _n, ri3) ->
    idents_of_raw_ident ri1 @ idents_of_raw_ident ri2 @ idents_of_raw_ident ri3
  | Raw_AuxList_comp (_, ri1, ri2, rcb) ->
    idents_of_raw_ident ri1 @ idents_of_raw_ident ri2
    @ idents_of_raw_comp_bound rcb
  | Raw_Union (_, m1, m2) ->
    idents_of_raw_mse m1 @ idents_of_raw_mse m2
  | Raw_Empty _ -> []

let idents_of_raw_bindspec (bs : raw_bindspec) : string list =
  match bs with
  | Raw_Bind (_, mse, ri) -> idents_of_raw_mse mse @ idents_of_raw_ident ri
  | Raw_AuxFnDef (_, ri, mse) -> idents_of_raw_ident ri @ idents_of_raw_mse mse
  | Raw_NamesEqual (_, m1, m2) -> idents_of_raw_mse m1 @ idents_of_raw_mse m2
  | Raw_NamesDistinct (_, m1, m2) -> idents_of_raw_mse m1 @ idents_of_raw_mse m2
  | Raw_AllNamesDistinct (_, m) -> idents_of_raw_mse m

let idents_of_raw_hom_spec_el (hse : raw_hom_spec_el) : string list =
  match hse with
  | Raw_hom_string _ -> []
  | Raw_hom_blanks _ -> []
  | Raw_hom_ident ris -> List.concat_map idents_of_raw_ident ris
  | Raw_hom_ident_dots (_l, ris1, _n, ris2) ->
    List.concat_map idents_of_raw_ident ris1
    @ List.concat_map idents_of_raw_ident ris2
  | Raw_hom_ident_comp (_l, ris, rcb) ->
    List.concat_map idents_of_raw_ident ris @ idents_of_raw_comp_bound rcb

let idents_of_raw_hom_for_scope_check ((_, rhs, _) : raw_homomorphism) : string list =
  List.concat_map idents_of_raw_hom_spec_el rhs

let idents_of_raw_hom_for_closure ((_, rhs, _) : raw_homomorphism) : string list =
  (* Hom bodies are backend-language text (even when tokenized into idents).
     They must not contribute Ott-level dependencies, otherwise backend library
     identifiers (e.g. Coq's [N]) can be mistaken for Ott declarations. *)
  ignore rhs;
  []

let idents_of_embed_spec_el (ese : embed_spec_el) : string list =
  match ese with
  | Embed_string (_, s) | Embed_inner (_, s) -> token_idents_in_string s

let idents_of_embed ((_, _hn, es) : raw_embed) : string list =
  List.concat_map idents_of_embed_spec_el es

let idents_of_semiraw_rule_for_closure (sr : semiraw_rule) : string list =
  let idents_of_parts parts =
    List.concat_map (fun (_l, s) -> token_idents_in_string s) parts
  in
  match sr with
  | Lineless_rule (_l, parts) -> idents_of_parts parts
  | Lined_rule (_l, above, _line, below) ->
    idents_of_parts above @ idents_of_parts below
  | Defncom (_l, es) -> List.concat_map idents_of_embed_spec_el es

let idents_of_semiraw_rule_for_scope_check (sr : semiraw_rule) : string list =
  let idents_of_parts parts =
    List.concat_map (fun (_l, s) -> token_idents_in_string s) parts
  in
  match sr with
  | Lineless_rule (_l, parts) -> idents_of_parts parts
  | Lined_rule (_l, above, _line, below) ->
    idents_of_parts above @ idents_of_parts below
  | Defncom (_l, _es) -> []

let idents_of_raw_funclause ((_l, s) : raw_funclause) : string list =
  token_idents_in_string s

module Raw_occurrences = struct
  type purpose =
    | For_closure
    | For_scope_check

  let hom_names purpose (raw_hom : raw_homomorphism) : string list =
    match purpose with
    | For_closure -> idents_of_raw_hom_for_closure raw_hom
    | For_scope_check -> idents_of_raw_hom_for_scope_check raw_hom

  let embed_names purpose (raw_embed : raw_embed) : string list =
    match purpose with
    | For_closure ->
      ignore raw_embed;
      []
    | For_scope_check -> idents_of_embed raw_embed

  let semiraw_rule_names purpose (sr : semiraw_rule) : string list =
    match purpose with
    | For_closure -> idents_of_semiraw_rule_for_closure sr
    | For_scope_check -> idents_of_semiraw_rule_for_scope_check sr

  let funclause_names _purpose (fc : raw_funclause) : string list =
    idents_of_raw_funclause fc

  let prod_names purpose (rp : raw_prod) : string list =
    let from_es = List.concat_map idents_of_raw_element rp.raw_prod_es in
    let from_bindspecs = List.concat_map idents_of_raw_bindspec rp.raw_prod_bs in
    let from_homs = List.concat_map (hom_names purpose) rp.raw_prod_homs in
    from_es @ from_bindspecs @ from_homs

  let rule_names purpose (rr : raw_rule) : string list =
    let from_prods = List.concat_map (prod_names purpose) rr.raw_rule_ps in
    let from_homs = List.concat_map (hom_names purpose) rr.raw_rule_homs in
    let from_embeds = List.concat_map (embed_names purpose) rr.raw_rule_embeds in
    from_prods @ from_homs @ from_embeds

  let metavar_names purpose (raw_md : raw_metavardefn) : string list =
    List.concat_map (hom_names purpose) raw_md.raw_mvd_rep

  let reln_defn_names purpose (rd : raw_defn) : string list =
    let from_es = List.concat_map idents_of_raw_element rd.raw_d_es in
    let from_body = List.concat_map (semiraw_rule_names purpose) rd.raw_d_body in
    let from_homs = List.concat_map (hom_names purpose) rd.raw_d_homs in
    from_es @ from_body @ from_homs

  let reln_defnclass_names purpose (rdc : raw_defnclass) : string list =
    let from_homs = List.concat_map (hom_names purpose) rdc.raw_dc_homs in
    let from_defns = List.concat_map (reln_defn_names purpose) rdc.raw_dc_defns in
    from_homs @ from_defns

  let fundefn_names purpose (rfd : raw_fundefn) : string list =
    let from_es = List.concat_map idents_of_raw_element rfd.raw_fd_es in
    let from_result = idents_of_raw_ident rfd.raw_fd_result in
    let from_clauses = List.concat_map (funclause_names purpose) rfd.raw_fd_clauses in
    let from_homs = List.concat_map (hom_names purpose) rfd.raw_fd_homs in
    from_es @ from_result @ from_clauses @ from_homs

  let fundefnclass_names purpose (rfdc : raw_fundefnclass) : string list =
    let from_homs = List.concat_map (hom_names purpose) rfdc.raw_fdc_homs in
    let from_fundefns = List.concat_map (fundefn_names purpose) rfdc.raw_fdc_fundefns in
    from_homs @ from_fundefns

  let subrule_names purpose (sr : raw_subrule) : string list =
    [sr.raw_sr_lower; sr.raw_sr_upper]
    @ List.concat_map (hom_names purpose) sr.raw_sr_homs

  let contextrule_names purpose (cr : raw_contextrule) : string list =
    [cr.raw_cr_ntr; cr.raw_cr_target; cr.raw_cr_hole]
    @ List.concat_map (hom_names purpose) cr.raw_cr_homs

  let subst_names purpose (sb : raw_subst) : string list =
    [sb.raw_sb_this; sb.raw_sb_that]
    @ List.concat_map (hom_names purpose) sb.raw_sb_homs

  let freevar_names purpose (fv : raw_freevar) : string list =
    [fv.raw_fv_this; fv.raw_fv_that]
    @ List.concat_map (hom_names purpose) fv.raw_fv_homs

  let referenced_names purpose (ri : raw_item) : StringSet.t =
    let names =
      match ri with
      | Raw_item_rs raw_rs -> List.concat_map (rule_names purpose) raw_rs
      | Raw_item_md raw_md -> metavar_names purpose raw_md
      | Raw_item_dcs (Raw_RDC rdc) -> reln_defnclass_names purpose rdc
      | Raw_item_dcs (Raw_FDC rfdc) -> fundefnclass_names purpose rfdc
      | Raw_item_srs raw_srs -> List.concat_map (subrule_names purpose) raw_srs
      | Raw_item_crs raw_crs -> List.concat_map (contextrule_names purpose) raw_crs
      | Raw_item_sbs raw_sbs -> List.concat_map (subst_names purpose) raw_sbs
      | Raw_item_fvs raw_fvs -> List.concat_map (freevar_names purpose) raw_fvs
      | Raw_item_embed raw_embeds -> List.concat_map (embed_names purpose) raw_embeds
      | Raw_item_pas _
      | Raw_item_hs _
      | Raw_item_coq_section _
      | Raw_item_coq_variable _
      | Raw_item_import _ -> []
    in
    StringSet.of_list names

  let referenced_names_for_closure (ri : raw_item) : StringSet.t =
    referenced_names For_closure ri

  let referenced_names_for_scope_check (ri : raw_item) : StringSet.t =
    referenced_names For_scope_check ri
end

let referenced_names_for_closure = Raw_occurrences.referenced_names_for_closure
let referenced_names_for_scope_check = Raw_occurrences.referenced_names_for_scope_check

let loc_of_item (ri : raw_item) : loc =
  match ri with
  | Raw_item_md md -> md.raw_mvd_loc
  | Raw_item_rs (rr :: _) -> rr.raw_rule_loc
  | Raw_item_rs [] -> dummy_loc
  | Raw_item_dcs (Raw_RDC rdc) -> rdc.raw_dc_loc
  | Raw_item_dcs (Raw_FDC rfdc) -> rfdc.raw_fdc_loc
  | Raw_item_srs (sr :: _) -> sr.raw_sr_loc
  | Raw_item_srs [] -> dummy_loc
  | Raw_item_crs (cr :: _) -> cr.raw_cr_loc
  | Raw_item_crs [] -> dummy_loc
  | Raw_item_sbs (sb :: _) -> sb.raw_sb_loc
  | Raw_item_sbs [] -> dummy_loc
  | Raw_item_fvs (fv :: _) -> fv.raw_fv_loc
  | Raw_item_fvs [] -> dummy_loc
  | Raw_item_embed ((l, _, _) :: _) -> l
  | Raw_item_embed [] -> dummy_loc
  | Raw_item_pas (pa :: _) -> pa.raw_pa_loc
  | Raw_item_pas [] -> dummy_loc
  | Raw_item_hs hs -> hs.raw_hs_loc
  | Raw_item_coq_section qs -> qs.raw_rqs_loc
  | Raw_item_coq_variable qv -> qv.raw_rqv_loc
  | Raw_item_import rid -> rid.rid_loc

let add_loaded_module_scope_infos
    (scope_info_by_file : (string, scope_info) Hashtbl.t)
    (state : resolver_state)
  : unit =
  Hashtbl.iter (fun canonical_path module_index ->
    Hashtbl.replace scope_info_by_file canonical_path module_index.module_scope_info
  ) state.loaded_modules

let build_dep_map (items : raw_item list) (synonym_map : (string * string) list)
  : (string, StringSet.t) Hashtbl.t =
  (* Ott has multiple namespaces. In particular, syntax-root synonym mapping
     (synonym -> primary) is valid ONLY for nonterm/metavar roots. Defn
     identifiers can legitimately share a surface string with syntax synonyms
     (e.g. a defn named "G" and a context synonym "G"). Dependency analysis
     must not collapse those namespaces, otherwise closure seeds cannot reach
     their dependencies. *)
  let syntax_names =
    StringSet.of_list (ntrs_of_items items @ mvrs_of_items items)
  in
  let other_names =
    StringSet.of_list
      (dcs_of_items items
       @ defn_idents_of_items items
       @ fundefn_idents_of_items items
       @ subst_names_of_items items
       @ freevar_names_of_items items)
  in
  let all_names = StringSet.union syntax_names other_names in
  let tbl : (string, StringSet.t) Hashtbl.t = Hashtbl.create 64 in
  let add_edge ~(from_key : string) ~(to_key : string) : unit =
    if from_key <> to_key then begin
      let existing = Hashtbl.find_opt tbl from_key |> Option.value ~default:StringSet.empty in
      Hashtbl.replace tbl from_key (StringSet.add to_key existing)
    end
  in

  (* Recognize Ott suffix structure in tokens like "vi" (root "v" with indexvar
     suffix "i") so importing a defn that mentions "vi" pulls in the declared
     nonterminal root "v" as a dependency. *)
  let roots_desc =
    List.sort
      (fun a b ->
        let la = String.length a and lb = String.length b in
        let c = compare lb la in
        if c <> 0 then c else compare a b)
      (StringSet.elements syntax_names)
  in
  let indexvars_desc_by_len =
    let acc = ref [] in
    List.iter (fun ri ->
      match ri with
      | Raw_item_md raw_md when raw_md.raw_mvd_indexvar ->
        List.iter (fun (n, _homs) -> acc := n :: !acc) raw_md.raw_mvd_names
      | _ -> ()
    ) items;
    List.sort
      (fun a b ->
        let la = String.length a and lb = String.length b in
        let c = compare lb la in
        if c <> 0 then c else compare a b)
      (List.sort_uniq String.compare !acc)
  in
  let parse_suffix_pieces (suffix : string) : string list option =
    let len = String.length suffix in
    if len = 0 then Some [] else
    let rec loop i acc =
      if i = len then Some (List.rev acc)
      else
        let c = suffix.[i] in
        if c >= '0' && c <= '9' then begin
          let j = ref (i + 1) in
          while !j < len && suffix.[!j] >= '0' && suffix.[!j] <= '9' do
            incr j
          done;
          loop !j acc
        end else if Auxl.issuffixpunct c then
          loop (i + 1) acc
        else begin
          let rec try_indexvars = function
            | [] -> None
            | iv :: rest ->
              let liv = String.length iv in
              if i + liv <= len && String.sub suffix i liv = iv then begin
                let j = i + liv in
                let has_minus1 =
                  j + 2 <= len && String.sub suffix j 2 = "-1"
                in
                let j' = if has_minus1 then j + 2 else j in
                Some (j', iv)
              end else
                try_indexvars rest
          in
          match try_indexvars indexvars_desc_by_len with
          | None -> None
          | Some (j, iv) -> loop j (iv :: acc)
        end
    in
    loop 0 []
  in
  let deps_of_token (s : string) : string list =
    if StringSet.mem s syntax_names then
      [resolve_to_primary synonym_map s]
    else if StringSet.mem s other_names then
      [s]
    else
      let slen = String.length s in
      let rec try_roots = function
        | [] -> []
        | root :: rest ->
          let rlen = String.length root in
          if slen > rlen && String.sub s 0 rlen = root then
            let suffix = String.sub s rlen (slen - rlen) in
            match parse_suffix_pieces suffix with
            | None -> try_roots rest
            | Some indexvars ->
              List.map (resolve_to_primary synonym_map) (root :: indexvars)
          else
            try_roots rest
      in
      try_roots roots_desc
  in

  (* Dependencies from grammar rules *)
  List.iter (fun ri ->
    match ri with
    | Raw_item_rs raw_rs ->
      List.iter (fun rr ->
        let primary = resolve_to_primary synonym_map rr.raw_rule_ntr_name in
        List.iter (fun rp ->
          let refs = Raw_occurrences.prod_names Raw_occurrences.For_closure rp in
          List.iter (fun s ->
            List.iter (fun d -> add_edge ~from_key:primary ~to_key:d) (deps_of_token s)
          ) refs
        ) rr.raw_rule_ps
      ) raw_rs
    | _ -> ()
  ) items;
  (* Dependencies from metavar/indexvar representations *)
  List.iter (fun ri ->
    match ri with
    | Raw_item_md raw_md ->
      let primary = resolve_to_primary synonym_map raw_md.raw_mvd_name in
      let refs = Raw_occurrences.metavar_names Raw_occurrences.For_closure raw_md in
      List.iter (fun s ->
        List.iter (fun d -> add_edge ~from_key:primary ~to_key:d) (deps_of_token s)
      ) refs
    | _ -> ()
  ) items;
  (* Subrule deps are bidirectional *)
  List.iter (fun ri ->
    match ri with
    | Raw_item_srs raw_srs ->
      List.iter (fun sr ->
        let lower = resolve_to_primary synonym_map sr.raw_sr_lower in
        let upper = resolve_to_primary synonym_map sr.raw_sr_upper in
        add_edge ~from_key:lower ~to_key:upper;
        add_edge ~from_key:upper ~to_key:lower
      ) raw_srs
    | _ -> ()
  ) items;
  (* Substitution/freevar declarations *)
  List.iter (fun ri ->
    match ri with
    | Raw_item_sbs raw_sbs ->
      List.iter (fun sb ->
        let refs = Raw_occurrences.subst_names Raw_occurrences.For_closure sb in
        List.iter (fun s ->
          List.iter (fun d -> add_edge ~from_key:sb.raw_sb_name ~to_key:d) (deps_of_token s)
        ) refs
      ) raw_sbs
    | Raw_item_fvs raw_fvs ->
      List.iter (fun fv ->
        let refs = Raw_occurrences.freevar_names Raw_occurrences.For_closure fv in
        List.iter (fun s ->
          List.iter (fun d -> add_edge ~from_key:fv.raw_fv_name ~to_key:d) (deps_of_token s)
        ) refs
      ) raw_fvs
    | _ -> ()
  ) items;
  (* Defn/fundefn dependencies.
     Track member-level dependencies to keep closure precise: importing one
     relation/function should not drag in unrelated siblings from the same
     class unless the class itself was explicitly imported. *)
  List.iter (fun ri ->
    match ri with
    | Raw_item_dcs rfrdc ->
      (match rfrdc with
       | Raw_RDC rdc ->
         let dc_name = rdc.raw_dc_name in
         List.iter (fun rd ->
           add_edge ~from_key:rd.raw_d_name ~to_key:dc_name;
           let refs = Raw_occurrences.reln_defn_names Raw_occurrences.For_closure rd in
           List.iter (fun s ->
             List.iter (fun d -> add_edge ~from_key:rd.raw_d_name ~to_key:d) (deps_of_token s)
           ) refs
         ) rdc.raw_dc_defns
       | Raw_FDC rfdc ->
         let fdc_name = rfdc.raw_fdc_name in
         List.iter (fun rfd ->
           add_edge ~from_key:rfd.raw_fd_name ~to_key:fdc_name;
           let refs = Raw_occurrences.fundefn_names Raw_occurrences.For_closure rfd in
           List.iter (fun s ->
             List.iter (fun d -> add_edge ~from_key:rfd.raw_fd_name ~to_key:d) (deps_of_token s)
           ) refs
         ) rfdc.raw_fdc_fundefns)
    | _ -> ()
  ) items;
  tbl

(* BFS transitive closure from a seed set *)
let transitive_closure (seeds : StringSet.t) (dep_map : (string, StringSet.t) Hashtbl.t)
  : StringSet.t =
  let visited = ref StringSet.empty in
  let queue = Queue.create () in
  StringSet.iter (fun s -> Queue.add s queue) seeds;
  while not (Queue.is_empty queue) do
    let name = Queue.pop queue in
    if not (StringSet.mem name !visited) then begin
      visited := StringSet.add name !visited;
      match Hashtbl.find_opt dep_map name with
      | None -> ()
      | Some deps ->
        StringSet.iter (fun d ->
          if not (StringSet.mem d !visited) then
            Queue.add d queue
        ) deps
    end
  done;
  !visited

(* ------------------------------------------------------------------ *)
(* Renaming pass over raw items                                        *)
(* ------------------------------------------------------------------ *)

type renamer =
  { rmap_tbl : (string, string) Hashtbl.t;
    roots_desc : string list;
    known_roots_desc : string list;
    indexvars_desc_by_len : string list;
  }

let indexvar_synonyms_of_items (items : raw_item list) : string list =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let acc = ref [] in
  let add (s : string) =
    if not (Hashtbl.mem seen s) then begin
      Hashtbl.add seen s ();
      acc := s :: !acc
    end
  in
  List.iter (fun ri ->
    match ri with
    | Raw_item_md raw_md when raw_md.raw_mvd_indexvar ->
      List.iter (fun (n, _homs) -> add n) raw_md.raw_mvd_names
    | _ -> ()
  ) items;
  List.rev !acc

let make_renamer ~(indexvars_desc : string list) ~(known_roots : string list)
    (rmap : (string * string) list) : renamer =
  (* Preserve List.assoc_opt semantics (first match wins). *)
  let rmap_tbl : (string, string) Hashtbl.t = Hashtbl.create (max 16 (List.length rmap)) in
  List.iter (fun (k, v) -> Hashtbl.replace rmap_tbl k v) (List.rev rmap);
  let roots_desc =
    List.sort
      (fun a b ->
        let la = String.length a and lb = String.length b in
        let c = compare lb la in
        if c <> 0 then c else compare a b)
      (List.sort_uniq String.compare (List.map fst rmap))
  in
  let known_roots_desc =
    List.sort
      (fun a b ->
        let la = String.length a and lb = String.length b in
        let c = compare lb la in
        if c <> 0 then c else compare a b)
      (List.sort_uniq String.compare known_roots)
  in
  let indexvars_desc_by_len =
    List.sort
      (fun a b ->
        let la = String.length a and lb = String.length b in
        let c = compare lb la in
        if c <> 0 then c else compare a b)
      indexvars_desc
  in
  { rmap_tbl; roots_desc; known_roots_desc; indexvars_desc_by_len }

let rename_root (ren : renamer) (s : string) : string =
  match Hashtbl.find_opt ren.rmap_tbl s with
  | Some s' -> s'
  | None -> s

type suffix_piece =
  | Sp_digits of string
  | Sp_punct of string
  | Sp_indexvar of string * bool (* name, has "-1" *)

let parse_suffix_pieces (ren : renamer) (suffix : string) : suffix_piece list option =
  let len = String.length suffix in
  if len = 0 then Some [] else
  let rec loop i acc =
    if i = len then Some (List.rev acc)
    else
      let c = suffix.[i] in
      if c >= '0' && c <= '9' then begin
        let j = ref (i + 1) in
        while !j < len && suffix.[!j] >= '0' && suffix.[!j] <= '9' do
          incr j
        done;
        let d = String.sub suffix i (!j - i) in
        loop !j (Sp_digits d :: acc)
      end else if Auxl.issuffixpunct c then
        loop (i + 1) (Sp_punct (String.make 1 c) :: acc)
      else begin
        let rec try_indexvars = function
          | [] -> None
          | iv :: rest ->
            let liv = String.length iv in
            if i + liv <= len && String.sub suffix i liv = iv then begin
              let j = i + liv in
              let has_minus1 =
                j + 2 <= len && String.sub suffix j 2 = "-1"
              in
              let j' = if has_minus1 then j + 2 else j in
              Some (j', Sp_indexvar (iv, has_minus1))
            end else
              try_indexvars rest
        in
        match try_indexvars ren.indexvars_desc_by_len with
        | None -> None
        | Some (j, piece) -> loop j (piece :: acc)
      end
  in
  loop 0 []

let rename_token (ren : renamer) (s : string) : string =
  match Hashtbl.find_opt ren.rmap_tbl s with
  | Some s' -> s'
  | None ->
    let slen = String.length s in
    let rec try_roots = function
      | [] -> s
      | root :: rest ->
        let rlen = String.length root in
        if slen > rlen && String.sub s 0 rlen = root then
          let suffix = String.sub s rlen (slen - rlen) in
          (match parse_suffix_pieces ren suffix with
           | None -> try_roots rest
           | Some pieces ->
             let root' = rename_root ren root in
             let buf = Buffer.create (String.length root' + String.length suffix) in
             Buffer.add_string buf root';
             List.iter (function
               | Sp_digits d -> Buffer.add_string buf d
               | Sp_punct p -> Buffer.add_string buf p
               | Sp_indexvar (iv, minus1) ->
                 Buffer.add_string buf (rename_root ren iv);
                 if minus1 then Buffer.add_string buf "-1"
             ) pieces;
             Buffer.contents buf)
        else
          try_roots rest
    in
    let renamed = try_roots ren.roots_desc in
    if renamed <> s then renamed
    else
      (* Handle renaming of suffix indexvars even when the root itself is not
         being renamed.

         This is crucial for internalization: an importing module can rename a
         nonterminal synonym (e.g. v :: q), producing tokens like "qi". Later,
         internalizing transitive-only indexvars must still rewrite the suffix
         part (i -> otttrans...), even though the root "q" is not in the
         internalization map. *)
      let rec try_known_roots = function
        | [] -> s
        | root :: rest ->
          let rlen = String.length root in
          if slen > rlen && String.sub s 0 rlen = root then
            let suffix = String.sub s rlen (slen - rlen) in
            match parse_suffix_pieces ren suffix with
            | None -> try_known_roots rest
            | Some pieces ->
              let root' = rename_root ren root in
              let changed = ref false in
              let buf = Buffer.create (String.length root' + String.length suffix) in
              Buffer.add_string buf root';
              List.iter (function
                | Sp_digits d -> Buffer.add_string buf d
                | Sp_punct p -> Buffer.add_string buf p
                | Sp_indexvar (iv, minus1) ->
                  let iv' = rename_root ren iv in
                  if iv' <> iv then changed := true;
                  Buffer.add_string buf iv';
                  if minus1 then Buffer.add_string buf "-1"
              ) pieces;
              let out = Buffer.contents buf in
              if !changed then out else try_known_roots rest
          else
            try_known_roots rest
      in
      try_known_roots ren.known_roots_desc

let rename_raw_ident (ren : renamer) ((l, s) : raw_ident) : raw_ident =
  (l, rename_token ren s)

let rename_raw_ident_root (ren : renamer) ((l, s) : raw_ident) : raw_ident =
  (l, rename_root ren s)

let rec rename_raw_element ren (e : raw_element) : raw_element =
  match e with
  | Raw_ident (l, ri) -> Raw_ident (l, rename_raw_ident ren ri)
  | Raw_option (l, es) -> Raw_option (l, List.map (rename_raw_element ren) es)
  | Raw_list (l, es) -> Raw_list (l, List.map (rename_raw_element ren) es)
  | Raw_nelist (l, es) -> Raw_nelist (l, List.map (rename_raw_element ren) es)
  | Raw_comp (l, es, rcb, ident_opt) ->
    Raw_comp (l,
      List.map (rename_raw_element ren) es,
      rename_raw_comp_bound ren rcb,
      Option.map (rename_raw_ident ren) ident_opt)
  | Raw_sugaroption (l, e) -> Raw_sugaroption (l, rename_raw_element ren e)
  | Raw_dots _ -> e

and rename_raw_comp_bound ren (rcb : raw_comp_bound) : raw_comp_bound =
  match rcb with
  | Raw_bound_comp (l, ri) -> Raw_bound_comp (l, rename_raw_ident ren ri)
  | Raw_bound_comp_u (l, ri1, ri2) ->
    Raw_bound_comp_u (l, rename_raw_ident ren ri1, rename_raw_ident ren ri2)
  | Raw_bound_comp_lu (l, ri1, ri2, n, ri3) ->
    Raw_bound_comp_lu (l, rename_raw_ident ren ri1, rename_raw_ident ren ri2, n, rename_raw_ident ren ri3)

let rec rename_raw_mse ren (mse : raw_mse) : raw_mse =
  match mse with
  | Raw_MVorNTExp (l, ri) -> Raw_MVorNTExp (l, rename_raw_ident ren ri)
  | Raw_MVorNTListExp_dotform (l, ri1, n, ri2) ->
    Raw_MVorNTListExp_dotform (l, rename_raw_ident ren ri1, n, rename_raw_ident ren ri2)
  | Raw_MVorNTListExp_comp (l, ri, rcb) ->
    Raw_MVorNTListExp_comp (l, rename_raw_ident ren ri, rename_raw_comp_bound ren rcb)
  | Raw_Aux (l, ri1, ri2) ->
    Raw_Aux (l, rename_raw_ident ren ri1, rename_raw_ident ren ri2)
  | Raw_AuxList_dotform (l, ri1, ri2, n, ri3) ->
    Raw_AuxList_dotform (l, rename_raw_ident ren ri1, rename_raw_ident ren ri2, n, rename_raw_ident ren ri3)
  | Raw_AuxList_comp (l, ri1, ri2, rcb) ->
    Raw_AuxList_comp (l, rename_raw_ident ren ri1, rename_raw_ident ren ri2, rename_raw_comp_bound ren rcb)
  | Raw_Union (l, m1, m2) -> Raw_Union (l, rename_raw_mse ren m1, rename_raw_mse ren m2)
  | Raw_Empty _ -> mse

let rename_raw_hom_spec_el ren (hse : raw_hom_spec_el) : raw_hom_spec_el =
  match hse with
  | Raw_hom_string s ->
    (* Hom strings are backend-language text; avoid renaming identifiers here.
       We only rename structured [[...]] occurrences (Raw_hom_ident forms). *)
    Raw_hom_string s
  | Raw_hom_blanks _ -> hse
  | Raw_hom_ident ris -> Raw_hom_ident (List.map (rename_raw_ident ren) ris)
  | Raw_hom_ident_dots (l, ris1, n, ris2) ->
    Raw_hom_ident_dots (l,
      List.map (rename_raw_ident ren) ris1, n,
      List.map (rename_raw_ident ren) ris2)
  | Raw_hom_ident_comp (l, ris, rcb) ->
    Raw_hom_ident_comp (l,
      List.map (rename_raw_ident ren) ris,
      rename_raw_comp_bound ren rcb)

let rename_raw_hom ren ((hn, rhs, l) : raw_homomorphism) : raw_homomorphism =
  (hn, List.map (rename_raw_hom_spec_el ren) rhs, l)

let rename_raw_bindspec ren (bs : raw_bindspec) : raw_bindspec =
  match bs with
  | Raw_Bind (l, mse, ri) -> Raw_Bind (l, rename_raw_mse ren mse, rename_raw_ident ren ri)
  | Raw_AuxFnDef (l, ri, mse) -> Raw_AuxFnDef (l, rename_raw_ident ren ri, rename_raw_mse ren mse)
  | Raw_NamesEqual (l, m1, m2) -> Raw_NamesEqual (l, rename_raw_mse ren m1, rename_raw_mse ren m2)
  | Raw_NamesDistinct (l, m1, m2) -> Raw_NamesDistinct (l, rename_raw_mse ren m1, rename_raw_mse ren m2)
  | Raw_AllNamesDistinct (l, m) -> Raw_AllNamesDistinct (l, rename_raw_mse ren m)

let rename_raw_prod ren (rp : raw_prod) : raw_prod =
  { rp with
    raw_prod_es = List.map (rename_raw_element ren) rp.raw_prod_es;
    raw_prod_homs = List.map (rename_raw_hom ren) rp.raw_prod_homs;
    raw_prod_bs = List.map (rename_raw_bindspec ren) rp.raw_prod_bs;
  }

let dedup_named_homs (xs : (string * raw_homomorphism list) list) :
    (string * raw_homomorphism list) list =
  (* Renaming can map multiple distinct synonyms to the same string (e.g. when
     a renamed import selects an existing synonym: `| ext :: E` where the rule
     is declared as `ext, E`). Downstream phases assume a raw ident has a
     unique lex, so we must merge duplicate entries here. *)
  let tbl : (string, raw_homomorphism list) Hashtbl.t = Hashtbl.create 8 in
  let order_rev : string list ref = ref [] in
  List.iter (fun (name, homs) ->
    match Hashtbl.find_opt tbl name with
    | None ->
      Hashtbl.add tbl name homs;
      order_rev := name :: !order_rev
    | Some existing ->
      Hashtbl.replace tbl name (existing @ homs)
  ) xs;
  let order = List.rev !order_rev in
  List.map (fun name -> (name, Hashtbl.find tbl name)) order

let rename_raw_rule ren (rr : raw_rule) : raw_rule =
  let renamed_names =
    List.map (fun (s, homs) ->
      (rename_root ren s, List.map (rename_raw_hom ren) homs)
    ) rr.raw_rule_ntr_names
  in
  let renamed_names = dedup_named_homs renamed_names in
  let renamed_primary =
    match renamed_names with
    | (s, _) :: _ -> s
    | [] -> rename_root ren rr.raw_rule_ntr_name
  in
  { rr with
    raw_rule_ntr_name = renamed_primary;
    raw_rule_ntr_names = renamed_names;
    raw_rule_pn_wrapper = rename_root ren rr.raw_rule_pn_wrapper;
    raw_rule_ps = List.map (rename_raw_prod ren) rr.raw_rule_ps;
    raw_rule_homs = List.map (rename_raw_hom ren) rr.raw_rule_homs;
  }

let rename_words_in_string ren s =
  (* Rename Ott identifiers inside semi-structured text by tokenization, while
     preserving all punctuation/whitespace verbatim.

     IMPORTANT: do not use naive space-splitting here. Many definition-rule
     lines begin with indentation, and wrappers can be empty strings; mapping
     "" would otherwise rewrite leading spaces into identifiers (e.g. "metal_").
  *)
  Auxl.map_ott_idents_in_string (rename_token ren) s

let rename_semiraw_rule ren (sr : semiraw_rule) : semiraw_rule =
  let rename_parts parts =
    List.map (fun (l, s) -> (l, rename_words_in_string ren s)) parts
  in
  match sr with
  | Lineless_rule (l, parts) ->
    Lineless_rule (l, rename_parts parts)
  | Lined_rule (l, above, line, below) ->
    Lined_rule (l, rename_parts above, line, rename_parts below)

let rename_raw_defn ren (rd : raw_defn) : raw_defn =
  { rd with
    raw_d_name = rename_root ren rd.raw_d_name;
    raw_d_wrapper = rename_root ren rd.raw_d_wrapper;
    raw_d_es = List.map (rename_raw_element ren) rd.raw_d_es;
    raw_d_categories = List.map (rename_raw_ident_root ren) rd.raw_d_categories;
    raw_d_homs = List.map (rename_raw_hom ren) rd.raw_d_homs;
    raw_d_body = List.map (rename_semiraw_rule ren) rd.raw_d_body;
  }

let rename_raw_funclause ren ((l, s) : raw_funclause) : raw_funclause =
  (l, rename_words_in_string ren s)

let rename_raw_fundefn ren (rfd : raw_fundefn) : raw_fundefn =
  { rfd with
    raw_fd_name = rename_root ren rfd.raw_fd_name;
    raw_fd_pn_wrapper = rename_root ren rfd.raw_fd_pn_wrapper;
    raw_fd_es = List.map (rename_raw_element ren) rfd.raw_fd_es;
    raw_fd_result = rename_raw_ident_root ren rfd.raw_fd_result;
    raw_fd_result_type = rename_root ren rfd.raw_fd_result_type;
    raw_fd_homs = List.map (rename_raw_hom ren) rfd.raw_fd_homs;
    raw_fd_clauses = List.map (rename_raw_funclause ren) rfd.raw_fd_clauses;
  }

let rename_in_items rmap (items : raw_item list) : raw_item list =
  let known_roots = ntrs_of_items items @ mvrs_of_items items in
  let ren =
    make_renamer
      ~indexvars_desc:(indexvar_synonyms_of_items items)
      ~known_roots
      rmap
  in
  List.map (fun ri ->
    match ri with
    | Raw_item_rs raw_rs ->
      Raw_item_rs (List.map (rename_raw_rule ren) raw_rs)
    | Raw_item_md raw_md ->
      let renamed_names =
        List.map (fun (s, homs) ->
          (rename_root ren s, List.map (rename_raw_hom ren) homs)
        ) raw_md.raw_mvd_names
      in
      let renamed_names = dedup_named_homs renamed_names in
      let renamed_primary =
        match renamed_names with
        | (s, _) :: _ -> s
        | [] -> rename_root ren raw_md.raw_mvd_name
      in
      Raw_item_md { raw_md with
        raw_mvd_name = renamed_primary;
        raw_mvd_names = renamed_names;
        raw_mvd_rep = List.map (rename_raw_hom ren) raw_md.raw_mvd_rep;
      }
    | Raw_item_srs raw_srs ->
      Raw_item_srs (List.map (fun sr ->
        { sr with
          raw_sr_lower = rename_root ren sr.raw_sr_lower;
          raw_sr_upper = rename_root ren sr.raw_sr_upper;
        }
      ) raw_srs)
    | Raw_item_crs raw_crs ->
      Raw_item_crs (List.map (fun cr ->
        { cr with
          raw_cr_ntr = rename_root ren cr.raw_cr_ntr;
          raw_cr_target = rename_root ren cr.raw_cr_target;
          raw_cr_hole = rename_root ren cr.raw_cr_hole;
        }
      ) raw_crs)
    | Raw_item_sbs raw_sbs ->
      Raw_item_sbs (List.map (fun sb ->
        { sb with
          raw_sb_this = rename_root ren sb.raw_sb_this;
          raw_sb_that = rename_root ren sb.raw_sb_that;
          raw_sb_name = rename_root ren sb.raw_sb_name;
        }
      ) raw_sbs)
    | Raw_item_fvs raw_fvs ->
      Raw_item_fvs (List.map (fun fv ->
        { fv with
          raw_fv_this = rename_root ren fv.raw_fv_this;
          raw_fv_that = rename_root ren fv.raw_fv_that;
          raw_fv_name = rename_root ren fv.raw_fv_name;
        }
      ) raw_fvs)
    | Raw_item_dcs raw_dcs ->
      Raw_item_dcs (match raw_dcs with
        | Raw_RDC rdc ->
          Raw_RDC { rdc with
            raw_dc_name = rename_root ren rdc.raw_dc_name;
            raw_dc_wrapper = rename_root ren rdc.raw_dc_wrapper;
            raw_dc_homs = List.map (rename_raw_hom ren) rdc.raw_dc_homs;
            raw_dc_defns = List.map (rename_raw_defn ren) rdc.raw_dc_defns;
          }
        | Raw_FDC rfdc ->
          Raw_FDC { rfdc with
            raw_fdc_name = rename_root ren rfdc.raw_fdc_name;
            raw_fdc_homs = List.map (rename_raw_hom ren) rfdc.raw_fdc_homs;
            raw_fdc_fundefns = List.map (rename_raw_fundefn ren) rfdc.raw_fdc_fundefns;
          })
    | _ -> ri
  ) items

(* ------------------------------------------------------------------ *)
(* Filtering: select items whose names are in the closure set          *)
(* ------------------------------------------------------------------ *)

let filter_items (included : StringSet.t) ~(explicit_seeds : StringSet.t)
    (synonym_map : (string * string) list)
    (items : raw_item list) : raw_item list =
  List.filter_map (fun ri ->
    match ri with
    | Raw_item_rs raw_rs ->
      let filtered = List.filter (fun rr ->
        StringSet.mem rr.raw_rule_ntr_name included
      ) raw_rs in
      if filtered = [] then None else Some (Raw_item_rs filtered)
    | Raw_item_md raw_md ->
      if StringSet.mem raw_md.raw_mvd_name included then Some ri
      else None
    | Raw_item_srs raw_srs ->
      let filtered = List.filter (fun sr ->
        StringSet.mem (resolve_to_primary synonym_map sr.raw_sr_lower) included
        && StringSet.mem (resolve_to_primary synonym_map sr.raw_sr_upper) included
      ) raw_srs in
      if filtered = [] then None else Some (Raw_item_srs filtered)
    | Raw_item_crs raw_crs ->
      let filtered = List.filter (fun cr ->
        StringSet.mem (resolve_to_primary synonym_map cr.raw_cr_ntr) included
        && StringSet.mem (resolve_to_primary synonym_map cr.raw_cr_target) included
        && StringSet.mem (resolve_to_primary synonym_map cr.raw_cr_hole) included
      ) raw_crs in
      if filtered = [] then None else Some (Raw_item_crs filtered)
    | Raw_item_sbs raw_sbs ->
      let filtered = List.filter (fun sb ->
        StringSet.mem sb.raw_sb_name included
        || (StringSet.mem (resolve_to_primary synonym_map sb.raw_sb_this) included
            && StringSet.mem (resolve_to_primary synonym_map sb.raw_sb_that) included)
      ) raw_sbs in
      if filtered = [] then None else Some (Raw_item_sbs filtered)
    | Raw_item_fvs raw_fvs ->
      let filtered = List.filter (fun fv ->
        StringSet.mem fv.raw_fv_name included
        || (StringSet.mem (resolve_to_primary synonym_map fv.raw_fv_this) included
            && StringSet.mem (resolve_to_primary synonym_map fv.raw_fv_that) included)
      ) raw_fvs in
      if filtered = [] then None else Some (Raw_item_fvs filtered)
    | Raw_item_pas _ ->
      (* Parsing annotations name productions, not import-layer declarations.
         Import resolution should preserve them verbatim and let the
         typechecker decide later whether they apply in the assembled grammar. *)
      Some ri
    | Raw_item_dcs (Raw_RDC rdc) ->
      let class_included = StringSet.mem rdc.raw_dc_name included in
      let class_explicit = StringSet.mem rdc.raw_dc_name explicit_seeds in
      let selected_defns =
        List.filter (fun rd -> StringSet.mem rd.raw_d_name included) rdc.raw_dc_defns
      in
      let keep_all_members =
        class_explicit || (class_included && selected_defns = [])
      in
      let defns = if keep_all_members then rdc.raw_dc_defns else selected_defns in
      if class_included || defns <> [] then
        Some (Raw_item_dcs (Raw_RDC { rdc with raw_dc_defns = defns }))
      else
        None
    | Raw_item_dcs (Raw_FDC rfdc) ->
      let class_included = StringSet.mem rfdc.raw_fdc_name included in
      let class_explicit = StringSet.mem rfdc.raw_fdc_name explicit_seeds in
      let selected_fundefns =
        List.filter (fun rfd -> StringSet.mem rfd.raw_fd_name included) rfdc.raw_fdc_fundefns
      in
      let keep_all_members =
        class_explicit || (class_included && selected_fundefns = [])
      in
      let fundefns = if keep_all_members then rfdc.raw_fdc_fundefns else selected_fundefns in
      if class_included || fundefns <> [] then
        Some (Raw_item_dcs (Raw_FDC { rfdc with raw_fdc_fundefns = fundefns }))
      else
        None
    (* Skip embeds and hom sections *)
    | Raw_item_embed _ -> None
    | Raw_item_hs _ -> None
    | Raw_item_import _ -> None
    | Raw_item_coq_section _ -> None
    | Raw_item_coq_variable _ -> None
  ) items

(* ------------------------------------------------------------------ *)
(* Hom block extraction from raw_homomorphism list                     *)
(* ------------------------------------------------------------------ *)

let normalize_import_hom_name = function
  | "coq" -> "rocq"
  | "coq-from" -> "rocq-from"
  | hn -> hn

let allowed_import_hom_names =
  ["rocq"; "rocq-from"; "isa"; "hol"; "lem"; "twf"; "ocaml"; "tex"]

let import_hom_payload ~loc ~name (rhs : raw_hom_spec) : string =
  let buf = Buffer.create 32 in
  let malformed = ref false in
  List.iter (function
    | Raw_hom_string s
    | Raw_hom_blanks s -> Buffer.add_string buf s
    | Raw_hom_ident _
    | Raw_hom_ident_dots _
    | Raw_hom_ident_comp _ -> malformed := true
  ) rhs;
  let value = String.trim (Buffer.contents buf) in
  if !malformed || value = "" then
    Auxl.error (Some loc)
      ("import hom \"" ^ name
       ^ "\" must be a single non-empty string\n");
  value

let extract_hom_pairs (homs : raw_homomorphism list) : (string * string) list =
  let seen : (string, loc) Hashtbl.t = Hashtbl.create (List.length homs) in
  List.map (fun ((hn, rhs, loc) : raw_homomorphism) ->
    let hn = normalize_import_hom_name (String.trim hn) in
    if hn = "menhir" then
      Auxl.error (Some loc)
        ("import-block menhir is no longer supported; "
         ^ "Menhir imports always use the {{ ocaml ... }} path\n");
    if not (List.mem hn allowed_import_hom_names) then
      Auxl.error (Some loc)
        ("unknown import hom \"" ^ hn ^ "\""
         ^ " (allowed import homs are "
         ^ String.concat ", " allowed_import_hom_names
         ^ ")\n");
    if Hashtbl.mem seen hn then
      Auxl.error (Some loc) ("duplicate import hom \"" ^ hn ^ "\"\n");
    Hashtbl.replace seen hn loc;
    (hn, import_hom_payload ~loc ~name:hn rhs)
  ) homs

(* ------------------------------------------------------------------ *)
(* Parsing a module file                                               *)
(* ------------------------------------------------------------------ *)

let parse_module_file (filename : string) : raw_item list =
  let c = open_in filename in
  let lexbuf = Lexing.from_channel c in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  let ris =
    try
      Grammar_parser.main (Grammar_lexer.my_lexer true Grammar_lexer.metalang) lexbuf
    with
    | My_parse_error (loc, s) ->
      close_in c;
      Auxl.error loc ("\n" ^ s ^ "\n")
  in
  close_in c;
  ris

(* ------------------------------------------------------------------ *)
(* Deduplication: remove items whose names are already known           *)
(* ------------------------------------------------------------------ *)

let filter_item_decl_groups (keep_decl : decl_group -> bool) (ri : raw_item)
  : raw_item option =
  let filter_list make_decl rebuild items =
    let kept = List.filter (fun item -> keep_decl (make_decl item)) items in
    if kept = [] then None else Some (rebuild kept)
  in
  match ri with
  | Raw_item_md raw_md ->
    if keep_decl (metavar_decl_group raw_md) then Some ri else None
  | Raw_item_rs raw_rs ->
    filter_list rule_decl_group (fun kept -> Raw_item_rs kept) raw_rs
  | Raw_item_dcs (Raw_RDC rdc) ->
    if keep_decl (reln_defnclass_decl_group rdc) then Some ri else None
  | Raw_item_dcs (Raw_FDC rfdc) ->
    if keep_decl (fundefnclass_decl_group rfdc) then Some ri else None
  | Raw_item_srs raw_srs ->
    filter_list subrule_decl_group (fun kept -> Raw_item_srs kept) raw_srs
  | Raw_item_crs raw_crs ->
    filter_list contextrule_decl_group (fun kept -> Raw_item_crs kept) raw_crs
  | Raw_item_sbs raw_sbs ->
    filter_list subst_decl_group (fun kept -> Raw_item_sbs kept) raw_sbs
  | Raw_item_fvs raw_fvs ->
    filter_list freevar_decl_group (fun kept -> Raw_item_fvs kept) raw_fvs
  | Raw_item_pas raw_pas ->
    filter_list parsing_decl_group (fun kept -> Raw_item_pas kept) raw_pas
  | _ -> Some ri

let dedup_items (known_names : StringSet.t) (items : raw_item list) : raw_item list =
  let keep_decl decl =
    not (should_suppress_decl_by_known_names ~known_names decl)
  in
  List.filter_map (filter_item_decl_groups keep_decl) items

let seed_decl_env_with_items
    (name_to_id : (string, string) Hashtbl.t)
    (items : raw_item list)
  : unit =
  let seed_local_name (name : string) =
    let id = "local:" ^ name in
    if not (Hashtbl.mem name_to_id name) then
      Hashtbl.replace name_to_id name id
  in
  List.iter (fun ri ->
    StringSet.iter seed_local_name (declared_names_of_item ri)
  ) items

let make_decl_group_claimer
    ~(name_to_id : (string, string) Hashtbl.t)
    ~(seen_decl_ids : (string, unit) Hashtbl.t)
  : decl_group -> bool =
  fun (decl : decl_group) ->
    if decl.allow_parallel_bindings then
      true
    else if Hashtbl.mem seen_decl_ids decl.decl_id then
      false
    else
      let conflict =
        List.find_opt (fun name -> Hashtbl.mem name_to_id name) decl.bind_names
      in
      match conflict with
      | Some name ->
        let prev = Auxl.the (Hashtbl.find_opt name_to_id name) in
        Auxl.error (Some decl.loc)
          ("Imported name '" ^ name ^ "' conflicts with an existing declaration.\n"
           ^ "  existing binding id: " ^ prev ^ "\n"
           ^ "  new binding id:      " ^ decl.decl_id ^ "\n")
      | None ->
        Hashtbl.replace seen_decl_ids decl.decl_id ();
        List.iter (fun name -> Hashtbl.replace name_to_id name decl.decl_id)
          decl.bind_names;
        true

let suppress_items_for_tc_env
    ~(claim_decl_group : decl_group -> bool)
    (items : raw_item list)
  : raw_item list =
  List.filter_map (filter_item_decl_groups claim_decl_group) items

let source_file_of_items ~default (ris : raw_item list) : string =
  match ris with
  | [] -> default
  | ri :: _ ->
    (match loc_of_item ri with
     | [] -> default
     | l :: _ -> l.Location.loc_start.Lexing.pos_fname)

(* ------------------------------------------------------------------ *)
(* Core: resolve a single import directive                             *)
(* ------------------------------------------------------------------ *)

let synonyms_of_primary synonym_map primary =
  List.filter_map (fun (syn, prim) ->
    if prim = primary then Some syn else None
  ) synonym_map

let provider_info_for_direct_import (module_index : module_index)
    (rid : raw_import_directive) : provider_info =
  make_provider_info
    ~module_name:rid.rid_module
    ~canonical_path:module_index.canonical_path
    ~homs:(extract_hom_pairs rid.rid_homs)
    ~loc:rid.rid_loc

let binding_kinds_for_origin ~state ~(provider : provider_info)
    ~(origin_name : string) : import_binding_kind list =
  match Hashtbl.find_opt state.loaded_modules provider.pi_file with
  | None -> []
  | Some module_index ->
    (match Hashtbl.find_opt module_index.binding_kinds_by_name origin_name with
     | None -> []
     | Some kinds -> kinds)

let add_export_kind (tbl : (string, export_kind list) Hashtbl.t)
    (name : string) (kind : export_kind) =
  let existing =
    match Hashtbl.find_opt tbl name with
    | Some kinds -> kinds
    | None -> []
  in
  if not (List.mem kind existing) then
    Hashtbl.replace tbl name (kind :: existing)

let add_binding_kind (tbl : (string, import_binding_kind list) Hashtbl.t)
    (name : string) (kind : import_binding_kind) =
  let existing =
    match Hashtbl.find_opt tbl name with
    | Some kinds -> kinds
    | None -> []
  in
  if not (List.mem kind existing) then
    Hashtbl.replace tbl name (kind :: existing)

let build_name_sources ~canonical_path (items : raw_item list)
  : (string, StringSet.t) Hashtbl.t =
  let tbl : (string, StringSet.t) Hashtbl.t = Hashtbl.create 64 in
  let source_file_of_loc (l : loc) : string =
    Import_naming.source_file_of_loc ~default:canonical_path l
  in
  let add_name_source name item_loc =
    let src = source_file_of_loc item_loc in
    let sources =
      match Hashtbl.find_opt tbl name with
      | Some existing -> existing
      | None -> StringSet.empty
    in
    Hashtbl.replace tbl name (StringSet.add src sources)
  in
  List.iter (fun ri ->
    List.iter (fun decl ->
      List.iter (fun name -> add_name_source name decl.loc) decl.source_names
    ) (decl_groups_of_item ri)
  ) items;
  tbl

let build_module_index ~canonical_path
    ~(expanded_items : raw_item list)
    ~(module_scope_info : scope_info)
    ~(nested_bindings : binding_info list)
    ~(nested_import_sites : import_site_info list)
  : module_index =
  let synonym_map = build_synonym_map expanded_items in
  let syntax_names =
    StringSet.of_list (ntrs_of_items expanded_items @ mvrs_of_items expanded_items)
  in
  let syntax_primaries =
    StringSet.of_list
      (ntr_primaries_of_items expanded_items @ mvr_primaries_of_items expanded_items)
  in
  let other_known_names =
    StringSet.of_list
      (dcs_of_items expanded_items
       @ defn_idents_of_items expanded_items
       @ fundefn_idents_of_items expanded_items
       @ subst_names_of_items expanded_items
       @ freevar_names_of_items expanded_items)
  in
  let export_kinds_by_name : (string, export_kind list) Hashtbl.t = Hashtbl.create 64 in
  let binding_kinds_by_name : (string, import_binding_kind list) Hashtbl.t = Hashtbl.create 64 in
  StringSet.iter (fun name ->
    add_export_kind export_kinds_by_name name Export_syntax;
    add_binding_kind binding_kinds_by_name name Import_syntax
  ) syntax_names;
  let class_members : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  let member_parent_class : (string, string) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun ri ->
    match ri with
    | Raw_item_dcs (Raw_RDC rdc) ->
      let members = List.map (fun rd -> rd.raw_d_name) rdc.raw_dc_defns in
      Hashtbl.replace class_members rdc.raw_dc_name members;
      add_export_kind export_kinds_by_name rdc.raw_dc_name Export_defnclass;
      add_binding_kind binding_kinds_by_name rdc.raw_dc_name Import_defnclass;
      List.iter (fun member_name ->
        Hashtbl.replace member_parent_class member_name rdc.raw_dc_name;
        add_export_kind export_kinds_by_name member_name Export_defn;
        add_binding_kind binding_kinds_by_name member_name Import_defn
      ) members
    | Raw_item_dcs (Raw_FDC rfdc) ->
      let members = List.map (fun rfd -> rfd.raw_fd_name) rfdc.raw_fdc_fundefns in
      Hashtbl.replace class_members rfdc.raw_fdc_name members;
      add_export_kind export_kinds_by_name rfdc.raw_fdc_name Export_fundefnclass;
      add_binding_kind binding_kinds_by_name rfdc.raw_fdc_name Import_fundefnclass;
      List.iter (fun member_name ->
        Hashtbl.replace member_parent_class member_name rfdc.raw_fdc_name;
        add_export_kind export_kinds_by_name member_name Export_fundefn;
        add_binding_kind binding_kinds_by_name member_name Import_fundefn
      ) members
    | Raw_item_sbs raw_sbs ->
      List.iter (fun sb ->
        add_export_kind export_kinds_by_name sb.raw_sb_name Export_subst;
        add_binding_kind binding_kinds_by_name sb.raw_sb_name Import_subst
      ) raw_sbs
    | Raw_item_fvs raw_fvs ->
      List.iter (fun fv ->
        add_export_kind export_kinds_by_name fv.raw_fv_name Export_freevar;
        add_binding_kind binding_kinds_by_name fv.raw_fv_name Import_freevar
      ) raw_fvs
    | _ -> ()
  ) expanded_items;
  let indexvar_names =
    let acc = ref StringSet.empty in
    List.iter (fun ri ->
      match ri with
      | Raw_item_md raw_md when raw_md.raw_mvd_indexvar ->
        List.iter (fun (name, _homs) ->
          acc := StringSet.add name !acc
        ) raw_md.raw_mvd_names
      | _ -> ()
    ) expanded_items;
    !acc
  in
  {
    canonical_path;
    expanded_items;
    module_scope_info;
    nested_bindings;
    nested_import_sites;
    synonym_map;
    syntax_names;
    syntax_primaries;
    other_known_names;
    export_kinds_by_name;
    binding_kinds_by_name;
    class_members;
    member_parent_class;
    dep_map = build_dep_map expanded_items synonym_map;
    name_sources = build_name_sources ~canonical_path expanded_items;
    indexvar_names;
  }

let lookup_nested_provider (module_index : module_index) (name : string)
  : provider_info option =
  let provider = ref None in
  List.iter (fun site ->
    List.iter (fun binding ->
      if binding.bi_local_name = name then
        match !provider with
        | None -> provider := Some binding.bi_provider
        | Some existing when provider_equiv existing binding.bi_provider -> ()
        | Some _ ->
          Auxl.error None
            ("Internal error: inconsistent nested import provider for name '"
             ^ name ^ "'.\n")
    ) site.isi_bindings
  ) module_index.nested_import_sites;
  !provider

let pick_non_syntax_export_kind (module_index : module_index) (name : string)
  : export_kind option =
  let kinds =
    match Hashtbl.find_opt module_index.export_kinds_by_name name with
    | Some kinds -> kinds
    | None -> []
  in
  let has kind = List.mem kind kinds in
  if has Export_defnclass then Some Export_defnclass
  else if has Export_fundefnclass then Some Export_fundefnclass
  else if has Export_defn then Some Export_defn
  else if has Export_fundefn then Some Export_fundefn
  else if has Export_subst then Some Export_subst
  else if has Export_freevar then Some Export_freevar
  else None

let validate_import_items ~rid ~(module_index : module_index) : validated_import_items =
  let seeds = ref StringSet.empty in
  let seen_in_block = ref StringSet.empty in
  let resolved_items_rev = ref [] in
  List.iter (fun (rii : raw_import_item) ->
    let in_ntr_mvr = StringSet.mem rii.rii_name module_index.syntax_names in
    let in_other = StringSet.mem rii.rii_name module_index.other_known_names in
    if in_ntr_mvr && in_other then
      (* Ott has multiple namespaces (syntax roots vs defns/subst/freevar/etc),
         but import items are written as bare strings. When a string is both:
         - a nonterminal/metavar *synonym*, and
         - another importable name (e.g. a defn identifier),
         we treat it as the syntax synonym by default.

         However, if it is a *primary* syntax root, the ambiguity is real and
         likely to create downstream confusion, so we still reject it. *)
      if StringSet.mem rii.rii_name module_index.syntax_primaries then
        Auxl.error (Some rii.rii_loc)
          ("Import item '" ^ rii.rii_name ^ "' is ambiguous in module '"
           ^ rid.rid_module ^ "'.\n"
           ^ "It matches both a nonterminal/metavar root and another importable name.\n");
    let resolved_name, export_kind =
      if in_ntr_mvr then
        (resolve_to_primary module_index.synonym_map rii.rii_name, Export_syntax)
      else
        match pick_non_syntax_export_kind module_index rii.rii_name with
        | Some kind -> (rii.rii_name, kind)
        | None ->
          Auxl.error (Some rii.rii_loc)
            ("Import item '" ^ rii.rii_name ^ "' not found in module '"
             ^ rid.rid_module ^ "'.\n")
    in
    if StringSet.mem resolved_name !seen_in_block then
      Auxl.error (Some rii.rii_loc)
        ("Duplicate import item '" ^ rii.rii_name ^ "' in import block for module '"
         ^ rid.rid_module ^ "'.\n");
    seen_in_block := StringSet.add resolved_name !seen_in_block;
    seeds := StringSet.add resolved_name !seeds;
    resolved_items_rev := {
      raw_item = rii;
      resolved_name;
      export_kind;
    } :: !resolved_items_rev
  ) rid.rid_items;
  {
    resolved_items = List.rev !resolved_items_rev;
    seeds = !seeds;
  }

let seed_closure_from_explicit_items ~(module_index : module_index)
    ~(validated_items : validated_import_items) : StringSet.t =
  (* If a class is imported explicitly, seed closure with all of its members
     so we get their full dependency closure without adding coarse class->all
     edges in the dependency graph. *)
  let closure_seeds = ref validated_items.seeds in
  List.iter (fun item ->
    match item.export_kind with
    | Export_defnclass | Export_fundefnclass ->
      (match Hashtbl.find_opt module_index.class_members item.resolved_name with
       | Some members ->
         List.iter (fun member_name ->
           closure_seeds := StringSet.add member_name !closure_seeds
         ) members
       | None -> ())
    | _ -> ()
  ) validated_items.resolved_items;
  !closure_seeds

let make_binding_info_for_origin
    ~state
    ~(provider : provider_info)
    ~(visibility : import_binding_visibility)
    ~(local_name : string)
    ~(origin_name : string)
  : binding_info =
  {
    bi_local_name = local_name;
    bi_origin_name = origin_name;
    bi_provider = provider;
    bi_visibility = visibility;
    bi_kinds =
      binding_kinds_for_origin ~state ~provider ~origin_name;
  }

let build_explicit_scope_and_bindings ~state ~direct_provider
    ~(module_index : module_index)
    ~(validated_items : validated_import_items)
  : explicit_import_view =
  let rename_map = ref [] in
  let root_links = ref [] in
  let add_rename from_name to_name =
    if not (List.exists (fun (x, y) -> x = from_name && y = to_name) !rename_map) then
      rename_map := (from_name, to_name) :: !rename_map
  in
  let add_root_link provider_root provider_primary local_root local_primary =
    let link =
      {
        irl_provider_root = provider_root;
        irl_provider_primary = provider_primary;
        irl_local_root = local_root;
        irl_local_primary = local_primary;
      }
    in
    if not (List.exists (fun link' -> link' = link) !root_links) then
      root_links := link :: !root_links
  in
  List.iter (fun item ->
    match item.raw_item.rii_rename with
    | None -> ()
    | Some new_name ->
      (match item.export_kind with
       | Export_syntax ->
         List.iter (fun syn -> add_rename syn new_name)
           (synonyms_of_primary module_index.synonym_map item.resolved_name)
       | _ ->
         add_rename item.raw_item.rii_name new_name)
  ) validated_items.resolved_items;

  (* Note: do NOT auto-prefix production wrappers for imported rules.
     Imported modules are compiled separately; changing wrapper-derived
     identifiers would make the importing module refer to names that do not
     exist in the imported module's backend output. *)
  let scope_names = ref StringSet.empty in
  let bindings = ref module_index.nested_bindings in
  let add_binding local_name origin_name =
    bindings :=
      make_binding_info_for_origin
        ~state
        ~provider:direct_provider
        ~visibility:Import_binding_explicit
        ~local_name
        ~origin_name
      :: !bindings
  in
  let add_scope_name name =
    scope_names := StringSet.add name !scope_names
  in
  let allowed_judgement_roots = ref StringSet.empty in
  List.iter (fun item ->
    let local_name =
      match item.raw_item.rii_rename with
      | Some renamed -> renamed
      | None -> item.resolved_name
    in
    match item.export_kind with
    | Export_syntax ->
      List.iter (fun provider_root ->
        let local_root =
          match item.raw_item.rii_rename with
          | Some _ -> local_name
          | None -> provider_root
        in
        add_root_link
          provider_root
          item.resolved_name
          local_root
          local_name
      ) (synonyms_of_primary module_index.synonym_map item.resolved_name);
      (match item.raw_item.rii_rename with
       | None ->
         List.iter (fun syn ->
           add_scope_name syn;
           add_binding syn item.resolved_name
         ) (synonyms_of_primary module_index.synonym_map item.resolved_name)
       | Some renamed ->
         add_scope_name renamed;
         add_binding renamed item.resolved_name)
    | Export_defnclass ->
      add_scope_name local_name;
      add_binding local_name item.resolved_name;
      allowed_judgement_roots :=
        StringSet.add item.resolved_name !allowed_judgement_roots;
      (match Hashtbl.find_opt module_index.class_members item.resolved_name with
       | Some members ->
         List.iter (fun member_name ->
           add_scope_name member_name;
           add_binding member_name member_name
         ) members
       | None -> ())
    | Export_fundefnclass ->
      add_scope_name local_name;
      add_binding local_name item.resolved_name;
      (match Hashtbl.find_opt module_index.class_members item.resolved_name with
       | Some members ->
         List.iter (fun member_name ->
           add_scope_name member_name;
           add_binding member_name member_name
         ) members
       | None -> ())
    | Export_defn ->
      add_scope_name local_name;
      add_binding local_name item.resolved_name;
      (match Hashtbl.find_opt module_index.member_parent_class item.resolved_name with
       | Some dc_name ->
         allowed_judgement_roots := StringSet.add dc_name !allowed_judgement_roots
       | None -> ())
    | Export_fundefn | Export_subst | Export_freevar ->
      add_scope_name local_name;
      add_binding local_name item.resolved_name
  ) validated_items.resolved_items;
  {
    rename_map = !rename_map;
    root_links = !root_links;
    scope_names = !scope_names;
    allowed_judgement_roots = !allowed_judgement_roots;
    bindings = !bindings;
  }

let strip_imported_defn_bodies (items : raw_item list) : raw_item list =
  (* Imported defn/fundefn bodies are implementations, not interfaces.
     Typechecking the importing module should not depend on imported bodies.

     Stripping bodies early is also important for hygiene renaming: we avoid
     any string-level rewriting in semiraw rule text (which can otherwise
     rename inside keywords like "in", "distinct", etc.). *)
  List.map (fun ri ->
    match ri with
    | Raw_item_dcs (Raw_RDC rdc) ->
      let defns =
        List.map (fun rd ->
          { rd with raw_d_body = []; raw_d_homs = [] }
        ) rdc.raw_dc_defns
      in
      Raw_item_dcs (Raw_RDC { rdc with raw_dc_homs = []; raw_dc_defns = defns })
    | Raw_item_dcs (Raw_FDC rfdc) ->
      let fundefns =
        List.map (fun rfd ->
          { rfd with raw_fd_clauses = []; raw_fd_homs = [] }
        ) rfdc.raw_fdc_fundefns
      in
      Raw_item_dcs (Raw_FDC { rfdc with raw_fdc_homs = []; raw_fdc_fundefns = fundefns })
    | _ -> ri
  ) items

let declared_typechecking_names (items : raw_item list) : StringSet.t =
  StringSet.of_list
    (ntrs_of_items items
     @ mvrs_of_items items
     @ dcs_of_items items
     @ defn_idents_of_items items
     @ fundefn_idents_of_items items
     @ subst_names_of_items items
     @ freevar_names_of_items items)

let internalize_transitive_names ~state ~(module_index : module_index)
    ~(fallback_provider : provider_info)
    ~(scope_names : StringSet.t)
    ~(allowed_judgement_roots : StringSet.t)
    ~(bindings : binding_info list)
    (items : raw_item list)
  : internalized_import_items =
  (* Names that are available for typechecking but not in the user's scope (§5.4). *)
  let declared_for_tc = declared_typechecking_names items in
  let transitive_only_names =
    StringSet.diff
      (StringSet.diff declared_for_tc scope_names)
      allowed_judgement_roots
  in
  let transitive_only_names =
    (* Idempotence: names we already internalized should remain stable across
       re-imports, otherwise import depth changes identifiers and breaks
       diamond-deduplication. *)
    StringSet.filter (fun name ->
      not (Import_naming.is_internal_transitive name)
      && not (Import_naming.is_synthesized_root name)
    ) transitive_only_names
  in
  let bindings = ref bindings in
  let source_file_for_name (name : string) : string =
    match Hashtbl.find_opt module_index.name_sources name with
    | None -> module_index.canonical_path
    | Some sources ->
      if StringSet.cardinal sources <= 1 then
        (match StringSet.elements sources with
         | src :: _ -> src
         | [] -> module_index.canonical_path)
      else
        Auxl.error None
          ("Imported name '" ^ name ^ "' is declared in multiple source files in the same import closure:\n"
           ^ "  " ^ String.concat "\n  " (StringSet.elements sources) ^ "\n")
  in
  StringSet.iter (fun name -> ignore (source_file_for_name name)) declared_for_tc;
  let internalize_name ~(src : string) ~(name : string) : string =
    Import_naming.internalize_name
      ~src
      ~is_indexvar:(fun n -> StringSet.mem n module_index.indexvar_names)
      ~name
  in
  let internalize_map =
    StringSet.fold (fun name acc ->
      let src = source_file_for_name name in
      let provider =
        match lookup_nested_provider module_index name with
        | Some provider -> provider
        | None -> fallback_provider
      in
      let internal_name = internalize_name ~src ~name in
      (* Record binding info so backends can qualify references to these hidden
         transitive-only names (they are not declared in the importing file). *)
      bindings :=
        make_binding_info_for_origin
          ~state
          ~provider
          ~visibility:Import_binding_transitive_hidden
          ~local_name:internal_name
          ~origin_name:name
        :: !bindings;
      (name, internal_name) :: acc
    ) transitive_only_names []
  in
  let items =
    if internalize_map = [] then items
    else rename_in_items internalize_map items
  in
  {
    items;
    transitive_only_names;
    bindings = !bindings;
  }

let namespace_imported_wrappers ~(direct_provider : provider_info)
    ~(bindings : binding_info list)
    (items : raw_item list)
  : raw_item list * binding_info list =
  (* Backend identifier namespacing for injected (imported) items.

     Many backends have a global identifier namespace per output file.
     Imported modules are compiled separately, so it is fine for imported
     productions/defns to collide with local ones in their *original*
     module output. However, during typechecking and pretty-printing in an
     importing module run, we need imported constructor/judgement ids to
     be unambiguous for internal lookup (e.g., prod_of_prodname), while
     still printing as qualified references to the original module.

     We achieve this by prefixing the various wrapper strings on injected
     rules/defns with a stable, per-source-file internal prefix and
     recording binding info that maps the internal ids back to their
     original names. *)
  let binding_by_local : (string, binding_info) Hashtbl.t = Hashtbl.create 128 in
  List.iter (fun binding ->
    Hashtbl.replace binding_by_local binding.bi_local_name binding
  ) bindings;
  let bindings_acc = ref [] in
  let add_internal_binding internal_name origin_name =
    if Hashtbl.mem binding_by_local internal_name then ()
    else begin
      let binding =
        {
          bi_local_name = internal_name;
          bi_origin_name = origin_name;
          bi_provider = direct_provider;
          bi_visibility = Import_binding_internal;
          bi_kinds = [];
        }
      in
      Hashtbl.replace binding_by_local internal_name binding;
      bindings_acc := binding :: !bindings_acc
    end
  in
  let orig_of_local local_name =
    match Hashtbl.find_opt binding_by_local local_name with
    | Some binding -> binding.bi_origin_name
    | None -> local_name
  in
  let namespace_wrapper wrapper =
    if Import_naming.is_internal_wrapper wrapper then wrapper
    else Import_naming.wrapper_prefix_for_module ~module_name:direct_provider.pi_module ^ wrapper
  in
  let items' =
    List.map (fun ri ->
      match ri with
      | Raw_item_rs raw_rs ->
        let raw_rs' =
          List.map (fun rr ->
            let orig_wrapper = rr.raw_rule_pn_wrapper in
            let wrapper' = namespace_wrapper orig_wrapper in
            (* Record provenance for production/constructor identifiers even
               when their parent nonterminal is not in user scope.

               These constructors can still appear in the importing module's
               generated backend output via:
               - transitive dependencies of imported defn forms, and
               - injected grammar fragments needed for parsing/typechecking.

               Without provenance, internal "__ott_mod_*" wrapper names leak
               into backend output and produce unbound identifiers (e.g. Coq
               cannot resolve "__ott_mod_*_val_ptr"). *)
            List.iter (fun rp ->
              let internal_prod = wrapper' ^ rp.raw_prod_name in
              let orig_prod = orig_wrapper ^ rp.raw_prod_name in
              add_internal_binding internal_prod orig_prod
            ) rr.raw_rule_ps;
            { rr with raw_rule_pn_wrapper = wrapper' }
          ) raw_rs
        in
        Raw_item_rs raw_rs'
      | Raw_item_dcs (Raw_RDC rdc) ->
        let orig_wrapper = rdc.raw_dc_wrapper in
        let wrapper' = namespace_wrapper orig_wrapper in
        List.iter (fun rd ->
          (* Record provenance for judgement-form production identifiers for
             *all* injected defns, not just those in user scope.

             Even hidden (transitive-only) judgements can appear inside
             imported rule bodies that the TeX backend inlines, and the core
             pretty-printers need a stable prodname mapping for internal lookup
             (e.g. prod_of_prodname). *)
          let local_defn = rd.raw_d_name in
          let orig_defn = orig_of_local local_defn in
          let internal_judgement = wrapper' ^ local_defn in
          let orig_judgement = orig_wrapper ^ orig_defn in
          add_internal_binding internal_judgement orig_judgement
        ) rdc.raw_dc_defns;
        Raw_item_dcs (Raw_RDC { rdc with raw_dc_wrapper = wrapper' })
      | Raw_item_dcs (Raw_FDC rfdc) ->
        let fundefns' =
          List.map (fun rfd ->
            let orig_wrapper = rfd.raw_fd_pn_wrapper in
            let wrapper' = namespace_wrapper orig_wrapper in
            (* See Raw_RDC case above: record provenance for hidden injected
               fundefn usage production identifiers too. *)
            let local_fn = rfd.raw_fd_name in
            let orig_fn = orig_of_local local_fn in
            let internal_fn = wrapper' ^ local_fn in
            let orig_fn_full = orig_wrapper ^ orig_fn in
            add_internal_binding internal_fn orig_fn_full;
            { rfd with raw_fd_pn_wrapper = wrapper' }
          ) rfdc.raw_fdc_fundefns
        in
        Raw_item_dcs (Raw_FDC { rfdc with raw_fdc_fundefns = fundefns' })
      | _ -> ri
    ) items
  in
  (items', !bindings_acc @ bindings)

let rec load_expanded_module_raw ~state ~base_dir ~search_paths
    ~keep_imported_defn_bodies ~loc ~module_name ~path
  : module_index =
  let canonical_path =
    resolve_module_file ~base_dir ~search_paths ~loc ~module_name path
  in
  claim_module_alias_path ~state ~loc ~module_name ~canonical_path;
  match Hashtbl.find_opt state.loaded_modules canonical_path with
  | Some module_index -> module_index
  | None ->
    if StringSet.mem canonical_path state.visiting then
      Auxl.error (Some loc)
        ("Circular import detected for module '" ^ module_name ^ "'.\n");
    state.visiting <- StringSet.add canonical_path state.visiting;
    let clear_visiting () =
      state.visiting <- StringSet.remove canonical_path state.visiting
    in
    let loaded_module =
      try
        let items = parse_module_file canonical_path in
        let items = Raw_hom_sections.apply_to_items items in
        let nested_base_dir = Filename.dirname canonical_path in
        let resolved =
          resolve_source_file
            ~state
            ~base_dir:nested_base_dir
            ~search_paths
            ~keep_imported_defn_bodies
            items
        in
        let is_internal_prov_name (name : string) : bool =
          Import_naming.is_internal_wrapper name
          || Import_naming.is_internal_transitive name
        in
        let child_bindings =
          List.concat_map (fun site ->
            List.filter (fun binding ->
              is_internal_prov_name binding.bi_local_name
            ) site.isi_bindings
          ) resolved.direct_sites
        in
        let expanded_items = resolved.imported_items @ resolved.local_items in
        let module_index =
          build_module_index
            ~canonical_path
            ~expanded_items
            ~module_scope_info:resolved.scope_info
            ~nested_bindings:child_bindings
            ~nested_import_sites:resolved.direct_sites
        in
        Hashtbl.replace state.loaded_modules canonical_path module_index;
        remember_imported_file state canonical_path;
        module_index
      with exn ->
        clear_visiting ();
        raise exn
    in
    clear_visiting ();
    loaded_module

and resolve_one_import ~state ~base_dir ~search_paths
    ~keep_imported_defn_bodies ~loc (rid : raw_import_directive)
  : resolved_import =
  let loaded_module =
    load_expanded_module_raw
      ~state
      ~base_dir
      ~search_paths
      ~keep_imported_defn_bodies
      ~loc
      ~module_name:rid.rid_module
      ~path:rid.rid_path
  in
  let validation =
    validate_import_items ~rid ~module_index:loaded_module
  in
  let closure_seeds =
    seed_closure_from_explicit_items
      ~module_index:loaded_module
      ~validated_items:validation
  in
  (* Compute transitive closure. Note: we do not force `formula` into the
     closure here; `formula`/`judgement` are synthesized/merged by the core
     typechecker. *)
  let closure = transitive_closure closure_seeds loaded_module.dep_map in
  let direct_provider =
    provider_info_for_direct_import loaded_module rid
  in
  let explicit_view =
    build_explicit_scope_and_bindings
      ~state
      ~direct_provider
      ~module_index:loaded_module
      ~validated_items:validation
  in
  let filtered =
    filter_items closure ~explicit_seeds:validation.seeds
      loaded_module.synonym_map loaded_module.expanded_items
  in
  let filtered_with_user_renames =
    if explicit_view.rename_map = [] then filtered
    else rename_in_items explicit_view.rename_map filtered
  in
  let filtered_for_typechecking =
    if keep_imported_defn_bodies then filtered_with_user_renames
    else strip_imported_defn_bodies filtered_with_user_renames
  in
  let internalized =
    internalize_transitive_names
      ~state
      ~module_index:loaded_module
      ~fallback_provider:direct_provider
      ~scope_names:explicit_view.scope_names
      ~allowed_judgement_roots:explicit_view.allowed_judgement_roots
      ~bindings:explicit_view.bindings
      filtered_for_typechecking
  in
  let injected_items, bindings =
    if keep_imported_defn_bodies then
      (internalized.items, internalized.bindings)
    else
      namespace_imported_wrappers
        ~direct_provider
        ~bindings:internalized.bindings
        internalized.items
  in
  let site_bindings = dedup_bindings bindings in
  {
    injected_items;
    site_info = {
      isi_provider = direct_provider;
      isi_bindings = site_bindings;
      isi_root_links = explicit_view.root_links;
    };
    scope_names = explicit_view.scope_names;
    allowed_judgement_roots = explicit_view.allowed_judgement_roots;
    transitive_only_names = internalized.transitive_only_names;
  }

and resolve_source_file ~state ~base_dir ~search_paths ~keep_imported_defn_bodies
    (ris : raw_item list)
  : resolved_source_file =
  let imported_items_rev = ref [] in
  let local_rename_acc = ref [] in
  let tc_transitive_names = ref StringSet.empty in
  let explicit_imported_names = ref StringSet.empty in
  let direct_sites_acc = ref [] in

  let source_file =
    source_file_of_items ~default:base_dir ris
  in

  let local_items0 = List.filter (fun ri ->
    match ri with Raw_item_import _ -> false | _ -> true
  ) ris in
  let local_name_to_id : (string, string) Hashtbl.t = Hashtbl.create 128 in
  let local_seen_decl_ids : (string, unit) Hashtbl.t = Hashtbl.create 128 in
  seed_decl_env_with_items local_name_to_id local_items0;
  let claim_decl_group =
    make_decl_group_claimer
      ~name_to_id:local_name_to_id
      ~seen_decl_ids:local_seen_decl_ids
  in

  let local_defnclass_roots =
    List.fold_left (fun acc ri ->
      match ri with
      | Raw_item_dcs (Raw_RDC rdc) -> StringSet.add rdc.raw_dc_name acc
      | _ -> acc
    ) StringSet.empty ris
  in
  let allowed_judgement_roots_acc = ref local_defnclass_roots in

  let local_declared_all =
    declared_names_of_items local_items0
  in

  List.iter (fun ri ->
    match ri with
    | Raw_item_import rid ->
      let resolved_import =
        resolve_one_import
          ~state
          ~keep_imported_defn_bodies
          ~base_dir ~search_paths ~loc:rid.rid_loc rid
      in
      direct_sites_acc := !direct_sites_acc @ [resolved_import.site_info];
      allowed_judgement_roots_acc :=
        StringSet.union !allowed_judgement_roots_acc
          resolved_import.allowed_judgement_roots;
      tc_transitive_names :=
        StringSet.union !tc_transitive_names
          resolved_import.transitive_only_names;

      let local_conflicts =
        StringSet.inter resolved_import.scope_names local_declared_all
      in
      if not (StringSet.is_empty local_conflicts) then (
        let name = StringSet.choose local_conflicts in
        Auxl.error (Some rid.rid_loc)
          ("Imported name '" ^ name ^ "' from module '" ^ rid.rid_module
           ^ "' conflicts with a local declaration.\n"
           ^ "Use a rename in the import block to avoid the collision.\n")
      );
      let import_conflicts =
        StringSet.inter resolved_import.scope_names !explicit_imported_names
      in
      if not (StringSet.is_empty import_conflicts) then (
        let name = StringSet.choose import_conflicts in
        Auxl.error (Some rid.rid_loc)
          ("Imported name '" ^ name ^ "' from module '" ^ rid.rid_module
           ^ "' conflicts with a name imported earlier.\n"
           ^ "Use a rename in one of the import blocks to avoid the collision.\n")
      );

      List.iter (fun (rii : raw_import_item) ->
        let local_name = match rii.rii_rename with Some r -> r | None -> rii.rii_name in
        local_rename_acc := (rid.rid_module ^ "#" ^ rii.rii_name, local_name) :: !local_rename_acc
      ) rid.rid_items;

      let injected_items =
        suppress_items_for_tc_env
          ~claim_decl_group
          resolved_import.injected_items
      in
      imported_items_rev := List.rev_append injected_items !imported_items_rev;

      explicit_imported_names :=
        StringSet.union !explicit_imported_names resolved_import.scope_names
    | _ -> ()
  ) ris;

  let local_items =
    if !local_rename_acc = [] then local_items0
    else rename_in_items !local_rename_acc local_items0
  in

  let allowed_user =
    StringSet.union local_declared_all !explicit_imported_names
  in
  let violations =
    List.filter_map (fun ri ->
      let refs = referenced_names_for_scope_check ri in
      let bad =
        StringSet.filter (fun n ->
          StringSet.mem n !tc_transitive_names && not (StringSet.mem n allowed_user)
        ) refs
      in
      if StringSet.is_empty bad then None else Some (ri, bad)
    ) local_items
  in
  (match violations with
   | [] -> ()
   | (ri, bad) :: _ ->
     let name = StringSet.choose bad in
     Auxl.error (Some (loc_of_item ri))
       ("Illegal reference to imported name '" ^ name ^ "'.\n"
        ^ "This name is only available transitively via imports.\n"
        ^ "If you want to use it, add it explicitly to an import block.\n"));

  let scope_info =
    {
      si_user_scope_names = StringSet.elements allowed_user;
      si_explicit_names = StringSet.elements !explicit_imported_names;
      si_transitive_hidden_names = StringSet.elements !tc_transitive_names;
      si_allowed_judgement_roots = StringSet.elements !allowed_judgement_roots_acc;
      si_direct_imports = !direct_sites_acc;
    }
  in
  {
    source_file;
    imported_items = List.rev !imported_items_rev;
    local_items;
    scope_info;
    direct_sites = !direct_sites_acc;
  }

(* ------------------------------------------------------------------ *)
(* Main entry point                                                    *)
(* ------------------------------------------------------------------ *)

let resolve_imports ~base_dirs ~search_paths ~keep_imported_defn_bodies
    (ris_per_file : raw_item list list) : raw_item list list * import_context =
  let state = activate_fresh_resolver_state () in
  let direct_providers_acc : provider_info list ref = ref [] in
  let scope_info_by_file : (string, scope_info) Hashtbl.t = Hashtbl.create 16 in

  let base_dirs =
    if List.length base_dirs <> List.length ris_per_file then
      Auxl.error None
        ("Internal error: resolve_imports called with "
         ^ string_of_int (List.length base_dirs)
         ^ " base_dirs for "
         ^ string_of_int (List.length ris_per_file)
         ^ " source files.\n")
    else base_dirs
  in

  (* Typechecking-environment state for suppressing diamond duplicates and
     detecting name conflicts from imported items. *)
  let tc_name_to_id : (string, string) Hashtbl.t = Hashtbl.create 256 in
  let tc_seen_decl_ids : (string, unit) Hashtbl.t = Hashtbl.create 256 in

  List.iter (fun ris ->
    seed_decl_env_with_items tc_name_to_id
      (List.filter (fun ri -> match ri with Raw_item_import _ -> false | _ -> true) ris)
  ) ris_per_file;

  let claim_decl_group =
    make_decl_group_claimer
      ~name_to_id:tc_name_to_id
      ~seen_decl_ids:tc_seen_decl_ids
  in

  let ris_per_file' = List.map2 (fun base_dir ris ->
    let resolved =
      resolve_source_file
        ~state
        ~base_dir
        ~search_paths
        ~keep_imported_defn_bodies
        ris
    in
    direct_providers_acc :=
      !direct_providers_acc
      @ List.map (fun site -> site.isi_provider) resolved.direct_sites;
    Hashtbl.replace scope_info_by_file resolved.source_file resolved.scope_info;
    let imported_items =
      suppress_items_for_tc_env
        ~claim_decl_group
        resolved.imported_items
    in
    imported_items @ resolved.local_items
  ) base_dirs ris_per_file in

  let direct_providers =
    dedup_providers_exact !direct_providers_acc
  in
  add_loaded_module_scope_infos scope_info_by_file state;

  let ic =
    import_context_of_scope_infos
      ~direct_providers
      ~imported_files:(imported_files ())
      scope_info_by_file
  in
  (ris_per_file', ic)
