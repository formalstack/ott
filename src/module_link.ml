(**************************************************************************)
(*                                   Ott                                  *)
(*                                                                        *)
(*  Module linking: provider-driven renaming + production provisioning     *)
(**************************************************************************)

open Types

type t = {
  local_primary_by_provider_primary : (string, string) Hashtbl.t;
  local_root_by_provider_root : (string, string) Hashtbl.t;
  ren_prodnames : (string, string) Hashtbl.t;
  provider_by_local_prod : (string, rule * prod) Hashtbl.t;
  provider_mvd_by_local_root : (string, metavardefn) Hashtbl.t;
  binding_additions : binding_info list;
}

let binding_additions (l : t) = l.binding_additions

let provider_by_local_prod (l : t) = l.provider_by_local_prod

let provider_mvd_by_local_root (l : t) = l.provider_mvd_by_local_root

let rename_opt (ren : (string, string) Hashtbl.t) (s : string) : string =
  match Hashtbl.find_opt ren s with
  | Some s' -> s'
  | None -> s

let rename_nonterm (ren : (string, string) Hashtbl.t) ((root, suff) : nonterm) : nonterm =
  (rename_opt ren root, suff)

let rename_metavar (ren : (string, string) Hashtbl.t) ((root, suff) : metavar) : metavar =
  (rename_opt ren root, suff)

let dedup_named_payloads (xs : (string * 'a list) list) : (string * 'a list) list =
  let tbl = Hashtbl.create 8 in
  let order_rev = ref [] in
  List.iter (fun (name, payload) ->
    match Hashtbl.find_opt tbl name with
    | None ->
      Hashtbl.add tbl name payload;
      order_rev := name :: !order_rev
    | Some existing ->
      Hashtbl.replace tbl name (existing @ payload)
  ) xs;
  let order = List.rev !order_rev in
  List.map (fun name -> (name, Hashtbl.find tbl name)) order

let strip_prefix ~(prefix : string) (s : string) : string option =
  let lp = String.length prefix and ls = String.length s in
  if ls >= lp && String.sub s 0 lp = prefix then
    Some (String.sub s lp (ls - lp))
  else
    None

let rename_rule_roots
    ~(local_primary_by_provider_primary : (string, string) Hashtbl.t)
    ~(local_root_by_provider_root : (string, string) Hashtbl.t)
    (r : rule) : rule =
  (* Keep semantic primaries and as-written root spellings separate; TeX
     linking needs both to preserve importer-surface names without breaking
     lookups that require primary roots. *)
  let rename_primary (ntr : nontermroot) =
    rename_opt local_primary_by_provider_primary ntr
  in
  let rename_root (ntr : nontermroot) =
    rename_opt local_root_by_provider_root ntr
  in
  {
    r with
    rule_ntr_name = rename_primary r.rule_ntr_name;
    rule_ntr_names =
      dedup_named_payloads
        (List.map (fun (n, hs) -> (rename_root n, hs)) r.rule_ntr_names);
    rule_pn_wrapper = rename_root r.rule_pn_wrapper;
  }

let rec rename_mse (local_root_by_provider_root : (string, string) Hashtbl.t) (m : mse) : mse =
  match m with
  | MetaVarExp mv -> MetaVarExp (rename_metavar local_root_by_provider_root mv)
  | NonTermExp nt -> NonTermExp (rename_nonterm local_root_by_provider_root nt)
  | MetaVarListExp (mv, b) -> MetaVarListExp (rename_metavar local_root_by_provider_root mv, b)
  | NonTermListExp (nt, b) -> NonTermListExp (rename_nonterm local_root_by_provider_root nt, b)
  | Aux (f, nt) -> Aux (f, rename_nonterm local_root_by_provider_root nt)
  | AuxList (f, nt, b) -> AuxList (f, rename_nonterm local_root_by_provider_root nt, b)
  | Union (m1, m2) ->
    Union
      (rename_mse local_root_by_provider_root m1,
       rename_mse local_root_by_provider_root m2)
  | Empty -> Empty

let rename_bindspec
    (local_root_by_provider_root : (string, string) Hashtbl.t)
    (bs : bindspec) : bindspec =
  match bs with
  | Bind (l, m, nt) ->
    Bind (l, rename_mse local_root_by_provider_root m, rename_nonterm local_root_by_provider_root nt)
  | AuxFnDef (l, f, m) -> AuxFnDef (l, f, rename_mse local_root_by_provider_root m)
  | NamesEqual (l, m1, m2) ->
    NamesEqual
      (l, rename_mse local_root_by_provider_root m1, rename_mse local_root_by_provider_root m2)
  | NamesDistinct (l, m1, m2) ->
    NamesDistinct
      (l, rename_mse local_root_by_provider_root m1, rename_mse local_root_by_provider_root m2)
  | AllNamesDistinct (l, m) -> AllNamesDistinct (l, rename_mse local_root_by_provider_root m)

let rec rename_element
    ~(local_primary_by_provider_primary : (string, string) Hashtbl.t)
    ~(local_root_by_provider_root : (string, string) Hashtbl.t)
    (e : element) : element =
  match e with
  | Lang_nonterm (ntr, nt) ->
    Lang_nonterm
      ( rename_opt local_primary_by_provider_primary ntr,
        rename_nonterm local_root_by_provider_root nt )
  | Lang_metavar (mvr, mv) ->
    Lang_metavar
      ( rename_opt local_primary_by_provider_primary mvr,
        rename_metavar local_root_by_provider_root mv )
  | Lang_terminal _ -> e
  | Lang_option es ->
    Lang_option
      (List.map
         (rename_element
            ~local_primary_by_provider_primary
            ~local_root_by_provider_root)
         es)
  | Lang_sugaroption _ -> e
  | Lang_list elb ->
    Lang_list
      {
        elb with
        elb_es =
          List.map
            (rename_element
               ~local_primary_by_provider_primary
               ~local_root_by_provider_root)
            elb.elb_es;
      }

let rename_prod
    ~(local_primary_by_provider_primary : (string, string) Hashtbl.t)
    ~(local_root_by_provider_root : (string, string) Hashtbl.t)
    ~(ren_prodnames : (string, string) Hashtbl.t)
    (p : prod) : prod =
  {
    p with
    prod_name = rename_opt ren_prodnames p.prod_name;
    prod_es =
      List.map
        (rename_element
           ~local_primary_by_provider_primary
           ~local_root_by_provider_root)
        p.prod_es;
    prod_bs = List.map (rename_bindspec local_root_by_provider_root) p.prod_bs;
  }

let build_root_maps
    ~(importer_ic : import_context)
    ~(importer_source_file : string)
    ~(provider_module : string)
    : ((string, string) Hashtbl.t * (string, string) Hashtbl.t) =
  let local_primary_by_provider_primary : (string, string) Hashtbl.t = Hashtbl.create 256 in
  let local_root_by_provider_root : (string, string) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun (link : import_root_link) ->
    if not (Hashtbl.mem local_root_by_provider_root link.irl_provider_root) then
      Hashtbl.replace local_root_by_provider_root link.irl_provider_root link.irl_local_root;
    if not (Hashtbl.mem local_primary_by_provider_primary link.irl_provider_primary) then
      Hashtbl.replace
        local_primary_by_provider_primary
        link.irl_provider_primary
        link.irl_local_primary
  ) (Import.root_links_in_source importer_ic
       ~source_file:importer_source_file
       ~provider_module);
  (local_primary_by_provider_primary, local_root_by_provider_root)

let record_root_metadata ~(default : string) (xd : syntaxdefn)
    : ((string, string) Hashtbl.t * (string, string) Hashtbl.t) =
  let src_by_root : (string, string) Hashtbl.t = Hashtbl.create 256 in
  let primary_by_root : (string, string) Hashtbl.t = Hashtbl.create 256 in
  let record (n : string) ~(primary : string) (l : loc) =
    let src = Import_naming.source_file_of_loc ~default l in
    (match Hashtbl.find_opt src_by_root n with
     | None -> Hashtbl.replace src_by_root n src
     | Some src0 ->
       if src0 = src then () else
         Auxl.error (Some l)
           ("Internal error: root '" ^ n ^ "' declared in multiple source files in provider syntax:\n"
            ^ "  " ^ src0 ^ "\n  " ^ src ^ "\n"));
    match Hashtbl.find_opt primary_by_root n with
    | None -> Hashtbl.replace primary_by_root n primary
    | Some primary0 ->
      if primary0 = primary then () else
        Auxl.error (Some l)
          ("Internal error: root '" ^ n ^ "' has inconsistent primaries in provider syntax:\n"
           ^ "  " ^ primary0 ^ "\n  " ^ primary ^ "\n")
  in
  List.iter (fun r ->
    List.iter (fun (n, _hs) -> record n ~primary:r.rule_ntr_name r.rule_loc) r.rule_ntr_names
  ) xd.xd_rs;
  List.iter (fun mvd ->
    List.iter (fun (n, _hs) -> record n ~primary:mvd.mvd_name mvd.mvd_loc) mvd.mvd_names
  ) xd.xd_mds;
  (src_by_root, primary_by_root)

let provider_for_file
    ~(importer_ic : import_context)
    ~(importer_source_file : string)
    ~(source_file : string)
  : provider_info =
  match Import.lookup_provider_by_file_in_source importer_ic ~source_file:importer_source_file source_file with
  | Some provider -> provider
  | None ->
    Auxl.error None
      ("Internal error: missing provider info while linking imported file '"
       ^ source_file ^ "'.\n")

let build
    ~(importer_ic : import_context)
    ~(importer_source_file : string)
    ~(provider_module : string)
    ~(provider_file : string)
    ~(provider_syntax : syntaxdefn)
  : t =
  let wrapper_prefix = Import_naming.wrapper_prefix_for_module ~module_name:provider_module in

  let indexvars : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun mvd ->
    if mvd.mvd_indexvar then
      List.iter (fun (n, _hs) -> Hashtbl.replace indexvars n ()) mvd.mvd_names
  ) provider_syntax.xd_mds;

  let local_primary_by_provider_primary, local_root_by_provider_root =
    build_root_maps ~importer_ic ~importer_source_file ~provider_module
  in

  let root_sources, provider_primary_by_root =
    record_root_metadata ~default:provider_file provider_syntax
  in
  let provider =
    provider_for_file
      ~importer_ic
      ~importer_source_file
      ~source_file:provider_file
  in

  let binding_additions_acc : binding_info list ref = ref [] in

  let ensure_root_mapping (orig : string) : unit =
    if Hashtbl.mem local_root_by_provider_root orig
       || Import_naming.is_synthesized_root orig
       || Import_naming.is_internal_transitive orig
    then ()
    else
      let src =
        match Hashtbl.find_opt root_sources orig with
        | Some s -> s
        | None -> provider_file
      in
      let local =
        Import_naming.internalize_name
          ~src
          ~is_indexvar:(fun n -> Hashtbl.mem indexvars n)
          ~name:orig
      in
      Hashtbl.replace local_root_by_provider_root orig local;
      (match Hashtbl.find_opt provider_primary_by_root orig with
       | Some provider_primary when provider_primary = orig ->
         if not (Hashtbl.mem local_primary_by_provider_primary provider_primary) then
           Hashtbl.replace local_primary_by_provider_primary provider_primary local
       | _ -> ());
      let provider =
        provider_for_file
          ~importer_ic
          ~importer_source_file
          ~source_file:src
      in
      binding_additions_acc :=
        {
          bi_local_name = local;
          bi_origin_name = orig;
          bi_provider = provider;
          bi_visibility = Import_binding_internal;
          bi_kinds = [Import_syntax];
        }
        :: !binding_additions_acc
  in

  (* Ensure we can rename any root mentioned by provider syntax (including
     synonyms), even if it wasn't included by the raw-token dependency closure
     when typechecking the importer. *)
  List.iter (fun r ->
    List.iter (fun (n, _hs) -> ensure_root_mapping n) r.rule_ntr_names
  ) provider_syntax.xd_rs;
  List.iter (fun mvd ->
    List.iter (fun (n, _hs) -> ensure_root_mapping n) mvd.mvd_names
  ) provider_syntax.xd_mds;

  let ren_prodnames : (string, string) Hashtbl.t = Hashtbl.create 1024 in
  let provider_by_local_prod : (string, rule * prod) Hashtbl.t = Hashtbl.create 1024 in
  let provider_mvd_by_local_root : (string, metavardefn) Hashtbl.t = Hashtbl.create 256 in

  let namespace_pnw (pnw : string) : string =
    if Import_naming.is_internal_wrapper pnw then pnw else wrapper_prefix ^ pnw
  in
  let strip_prefix_or_self ~(prefix : string) (s : string) : string =
    match strip_prefix ~prefix s with
    | Some rest -> rest
    | None -> s
  in

  List.iter (fun r ->
    let pnw = r.rule_pn_wrapper in
    let pnw_local = namespace_pnw pnw in
    let r_local =
      let r' =
        rename_rule_roots
          ~local_primary_by_provider_primary
          ~local_root_by_provider_root
          r
      in
      { r' with rule_pn_wrapper = pnw_local }
    in
    List.iter (fun p ->
      let raw = strip_prefix_or_self ~prefix:pnw p.prod_name in
      let local_pn = pnw_local ^ raw in
      Hashtbl.replace ren_prodnames p.prod_name local_pn;
      binding_additions_acc :=
        {
          bi_local_name = local_pn;
          bi_origin_name = p.prod_name;
          bi_provider = provider;
          bi_visibility = Import_binding_internal;
          bi_kinds = [];
        }
        :: !binding_additions_acc;
      let p_local =
        rename_prod
          ~local_primary_by_provider_primary
          ~local_root_by_provider_root
          ~ren_prodnames
          p
      in
      if not (Hashtbl.mem provider_by_local_prod p_local.prod_name) then
        Hashtbl.replace provider_by_local_prod p_local.prod_name (r_local, p_local)
    ) r.rule_ps
  ) provider_syntax.xd_rs;

  List.iter (fun mvd ->
    let mvd_names =
      dedup_named_payloads
        (List.map
           (fun (n, hs) -> (rename_opt local_root_by_provider_root n, hs))
           mvd.mvd_names)
    in
    let mvd' =
      {
        mvd with
        mvd_name = rename_opt local_primary_by_provider_primary mvd.mvd_name;
        mvd_names;
      }
    in
    List.iter (fun (n, _hs) ->
      if not (Hashtbl.mem provider_mvd_by_local_root n) then
        Hashtbl.replace provider_mvd_by_local_root n mvd'
    ) mvd'.mvd_names
  ) provider_syntax.xd_mds;

  { local_primary_by_provider_primary;
    local_root_by_provider_root;
    ren_prodnames;
    provider_by_local_prod;
    provider_mvd_by_local_root;
    binding_additions = !binding_additions_acc }

let rec rename_symterm (l : t) (st : symterm) : symterm =
  match st with
  | St_node (loc, nb) ->
    let nb' =
      {
        nb with
        st_rule_ntr_name =
          rename_opt l.local_primary_by_provider_primary nb.st_rule_ntr_name;
        st_prod_name = rename_opt l.ren_prodnames nb.st_prod_name;
        st_es = List.map (rename_symterm_element l) nb.st_es;
      }
    in
    St_node (loc, nb')
  | St_nonterm (loc, ntr, nt) ->
    St_nonterm
      ( loc,
        rename_opt l.local_primary_by_provider_primary ntr,
        rename_nonterm l.local_root_by_provider_root nt )
  | St_nontermsub (loc, lower, top, nt) ->
    St_nontermsub
      ( loc,
        rename_opt l.local_primary_by_provider_primary lower,
        rename_opt l.local_primary_by_provider_primary top,
        rename_nonterm l.local_root_by_provider_root nt )
  | St_uninterpreted _ -> st

and rename_symterm_element (l : t) (e : symterm_element) : symterm_element =
  match e with
  | Ste_st (loc, st) -> Ste_st (loc, rename_symterm l st)
  | Ste_metavar (loc, mvr, mv) ->
    Ste_metavar
      ( loc,
        rename_opt l.local_primary_by_provider_primary mvr,
        rename_metavar l.local_root_by_provider_root mv )
  | Ste_var (loc, mvr, v) ->
    Ste_var (loc, rename_opt l.local_primary_by_provider_primary mvr, v)
  | Ste_list (loc, items) -> Ste_list (loc, List.map (rename_symterm_list_item l) items)

and rename_symterm_list_item (l : t) (it : symterm_list_item) : symterm_list_item =
  match it with
  | Stli_single (loc, es) -> Stli_single (loc, List.map (rename_symterm_element l) es)
  | Stli_listform stlb ->
    Stli_listform { stlb with stl_elements = List.map (rename_symterm_element l) stlb.stl_elements }
