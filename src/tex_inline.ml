(**************************************************************************)
(*                                   Ott                                  *)
(*                                                                        *)
(*  TeX backend import inlining/link step                                 *)
(**************************************************************************)

open Types

module StringSet = Set.Make (String)

(* Cache compiled module bodies: canonical_path -> processed relationsdefn *)
let compiled_relations_cache : (string, relationsdefn) Hashtbl.t = Hashtbl.create 16
let compiled_syntax_cache : (string, syntaxdefn) Hashtbl.t = Hashtbl.create 16

let clone_import_context (ic : import_context) : import_context =
  {
    ic_direct_providers = ic.ic_direct_providers;
    ic_imported_files = ic.ic_imported_files;
    ic_binding_info_by_local_name = Hashtbl.copy ic.ic_binding_info_by_local_name;
    ic_scope_info_by_source_file = Hashtbl.copy ic.ic_scope_info_by_source_file;
  }

let empty_import_context () : import_context =
  {
    ic_direct_providers = [];
    ic_imported_files = [];
    ic_binding_info_by_local_name = Hashtbl.create 0;
    ic_scope_info_by_source_file = Hashtbl.create 0;
  }

let sort_homs (homs : (string * string) list) : (string * string) list =
  List.sort compare homs

let strip_prefix ~(prefix : string) (s : string) : string option =
  let lp = String.length prefix and ls = String.length s in
  if ls >= lp && String.sub s 0 lp = prefix then
    Some (String.sub s lp (ls - lp))
  else
    None

let rewrite_drule_name
    ~(src_defnclass_wrapper : string)
    ~(src_defn_wrapper : string)
    ~(dst_defnclass_wrapper : string)
    ~(dst_defn_wrapper : string)
    (name : string) : string =
  let pref = src_defnclass_wrapper ^ src_defn_wrapper in
  match strip_prefix ~prefix:pref name with
  | None -> name
  | Some suffix -> dst_defnclass_wrapper ^ dst_defn_wrapper ^ suffix

type compiled_index = {
  (* Keyed by the provider's judgement-form production name:
       dc.dc_wrapper ^ d.d_name
     This is the stable, globally-unique identity for a defn body (unlike
     [d.d_name], which can be reused across defnclasses). *)
  defns : (string, (string (*dc_wrapper*) * string (*d_wrapper*) * processed_semiraw_rule list)) Hashtbl.t;
  (* Keyed by the provider's function-form production name:
       fd.fd_pn_wrapper ^ fd.fd_name
     (Similarly avoids collisions when fd_name is reused.) *)
  fundefns : (string, fundefn) Hashtbl.t;
}

let index_compiled_relations (rels : relationsdefn) : compiled_index =
  let defns = Hashtbl.create 64 in
  let fundefns = Hashtbl.create 64 in
  List.iter (function
    | RDC dc ->
      List.iter (fun d ->
        let key = dc.dc_wrapper ^ d.d_name in
        Hashtbl.replace defns key (dc.dc_wrapper, d.d_wrapper, d.d_rules)
      ) dc.dc_defns
    | FDC fdc ->
      List.iter (fun fd ->
        let key = fd.fd_pn_wrapper ^ fd.fd_name in
        Hashtbl.replace fundefns key fd
      ) fdc.fdc_fundefns
  ) rels;
  { defns; fundefns }

let prodname_set_of_syntax (xd : syntaxdefn) : StringSet.t =
  List.fold_left (fun acc r ->
    List.fold_left (fun acc p -> StringSet.add p.prod_name acc) acc r.rule_ps
  ) StringSet.empty xd.xd_rs

let rec prodnames_of_symterm (acc : StringSet.t) (st : symterm) : StringSet.t =
  match st with
  | St_node (_l, nb) ->
    let acc = StringSet.add nb.st_prod_name acc in
    List.fold_left prodnames_of_symterm_element acc nb.st_es
  | St_nonterm _ | St_nontermsub _ | St_uninterpreted _ -> acc

and prodnames_of_symterm_element (acc : StringSet.t) (e : symterm_element) : StringSet.t =
  match e with
  | Ste_st (_l, st) -> prodnames_of_symterm acc st
  | Ste_metavar _ | Ste_var _ -> acc
  | Ste_list (_l, items) ->
    List.fold_left prodnames_of_symterm_list_item acc items

and prodnames_of_symterm_list_item (acc : StringSet.t) (it : symterm_list_item) : StringSet.t =
  match it with
  | Stli_single (_l, es) -> List.fold_left prodnames_of_symterm_element acc es
  | Stli_listform stlb -> List.fold_left prodnames_of_symterm_element acc stlb.stl_elements

let prodnames_of_relations (rels : relationsdefn) : StringSet.t =
  List.fold_left (fun acc frdc ->
    match frdc with
    | RDC dc ->
      List.fold_left (fun acc d ->
        let acc = prodnames_of_symterm acc d.d_form in
        List.fold_left (fun acc psr ->
          match psr with
          | PSR_Defncom _ -> acc
          | PSR_Rule dr ->
            let acc =
              List.fold_left (fun acc (_hn, st) -> prodnames_of_symterm acc st)
                acc dr.drule_premises
            in
            prodnames_of_symterm acc dr.drule_conclusion
        ) acc d.d_rules
      ) acc dc.dc_defns
    | FDC fdc ->
      List.fold_left (fun acc fd ->
        let acc = prodnames_of_symterm acc fd.fd_form in
        List.fold_left (fun acc fc ->
          let acc = prodnames_of_symterm acc fc.fc_lhs in
          prodnames_of_symterm acc fc.fc_rhs
        ) acc fd.fd_clauses
      ) acc fdc.fdc_fundefns
  ) StringSet.empty rels

let merge_prod_into_syntax (xd : syntaxdefn) ~(src_rule : rule) ~(src_prod : prod) : syntaxdefn =
  let rec go acc = function
    | [] ->
      (* Rule missing entirely: add a minimal rule containing the needed prod. *)
      let r' = { src_rule with rule_ps = [src_prod] } in
      List.rev (r' :: acc)
    | r :: rs ->
      if r.rule_ntr_name = src_rule.rule_ntr_name then
        if List.exists (fun p -> p.prod_name = src_prod.prod_name) r.rule_ps then
          List.rev_append acc (r :: rs)
        else
          let r' = { r with rule_ps = r.rule_ps @ [src_prod] } in
          List.rev_append acc (r' :: rs)
      else
        go (r :: acc) rs
  in
  { xd with xd_rs = go [] xd.xd_rs }

let compile_module_relations
    ~(m_tex : pp_mode)
    ~(quotient_rules : bool)
    ~(generate_aux_rules : bool)
    ~(targets_non_tex : string list)
    ~(merge_fragments : bool)
    ~(module_file : string)
    (items : raw_item list) : relationsdefn =
  match Hashtbl.find_opt compiled_relations_cache module_file with
  | Some rels -> rels
  | None ->
    let items =
      List.filter (function Raw_item_import _ -> false | _ -> true) items
    in
    let import_ctx = empty_import_context () in
    let (xd, _structure, rdcs) =
      Grammar_typecheck.check_and_disambiguate
        m_tex quotient_rules generate_aux_rules targets_non_tex [module_file]
        merge_fragments false import_ctx [items]
    in
    Hashtbl.replace compiled_syntax_cache module_file xd;
    let lookup = Term_parser.make_parser xd in
    let rels = Defns.process_raw_defnclasss m_tex xd lookup rdcs in
    Hashtbl.replace compiled_relations_cache module_file rels;
    rels

let inline_imported_defn_bodies
    ~(m_tex : pp_mode)
    ~(quotient_rules : bool)
    ~(generate_aux_rules : bool)
    ~(targets_non_tex : string list)
    ~(merge_fragments : bool)
    (sd : systemdefn) : systemdefn =
  let ic = clone_import_context sd.syntax.xd_imports in
  let syntax0 = { sd.syntax with xd_imports = ic } in

  let same_binding (left : binding_info) (right : binding_info) : bool =
    left.bi_local_name = right.bi_local_name
    && left.bi_origin_name = right.bi_origin_name
    && left.bi_provider.pi_module = right.bi_provider.pi_module
    && left.bi_provider.pi_file = right.bi_provider.pi_file
    && left.bi_provider.pi_default_id = right.bi_provider.pi_default_id
    && sort_homs left.bi_provider.pi_homs = sort_homs right.bi_provider.pi_homs
    && left.bi_visibility = right.bi_visibility
    && List.sort compare left.bi_kinds = List.sort compare right.bi_kinds
  in
  let add_unique_binding table (binding : binding_info) =
    let existing =
      match Hashtbl.find_opt table binding.bi_local_name with
      | Some bindings -> bindings
      | None -> []
    in
    if not (List.exists (fun binding' -> same_binding binding binding') existing) then
      Hashtbl.replace table binding.bi_local_name (binding :: existing)
  in
  let bindings_by_local_name : (string, binding_info list) Hashtbl.t =
    Hashtbl.create 64
  in
  Hashtbl.iter (fun _source_file scope ->
    List.iter (fun site ->
      List.iter (add_unique_binding bindings_by_local_name) site.isi_bindings
    ) scope.si_direct_imports
  ) ic.ic_scope_info_by_source_file;
  let bindings_for_local_name (local_name : string) : binding_info list =
    match Hashtbl.find_opt bindings_by_local_name local_name with
    | Some bindings -> List.rev bindings
    | None -> []
  in
  let add_fallback_binding_if_absent (binding : binding_info) : unit =
    add_import_binding_if_absent ic binding
  in

  (* Map direct provider module name -> canonical module file path. *)
  let module_file : (string, string) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun provider ->
    Hashtbl.replace module_file provider.pi_module provider.pi_file
  ) (Import.direct_providers ic);

  (* Only compile modules we actually need bodies from.

     The TeX “link” step should be conservative: if the importing file did not
     pull in any imported defn/fundefn declarations (or if none of those
     declarations need bodies), we must not compile imported modules at all.

     This avoids TeX-only failures in unused parts of imported modules (e.g.
     meta/sugar productions missing a TeX hom), and matches the separate-
     compilation model used by typechecking. *)
  let modules_needing_bodies : StringSet.t =
    let needed = ref StringSet.empty in
    let mark_if_imported_and_bodyless (name : string) ~(is_empty : bool) =
      if is_empty then
        List.iter (fun binding ->
          needed := StringSet.add binding.bi_provider.pi_module !needed
        ) (bindings_for_local_name name)
    in
    List.iter (function
      | RDC dc ->
        List.iter (fun d ->
          mark_if_imported_and_bodyless d.d_name ~is_empty:(d.d_rules = [])
        ) dc.dc_defns
      | FDC fdc ->
        List.iter (fun fd ->
          mark_if_imported_and_bodyless fd.fd_name ~is_empty:(fd.fd_clauses = [])
        ) fdc.fdc_fundefns
    ) sd.relations;
    !needed
  in

  (* Compile each needed module once (from Import's expanded module cache). *)
  let compiled_by_module : (string, compiled_index) Hashtbl.t = Hashtbl.create 16 in
  Hashtbl.iter (fun m file ->
    if StringSet.mem m modules_needing_bodies then
      match Import.get_loaded_module_items file with
      | None -> ()
      | Some items ->
        let rels =
          compile_module_relations
            ~m_tex ~quotient_rules ~generate_aux_rules ~targets_non_tex ~merge_fragments
            ~module_file:file items
        in
        Hashtbl.replace compiled_by_module m (index_compiled_relations rels)
  ) module_file;

  (* Provider-driven module link contexts for renaming imported bodies and for
     supplying productions referenced by inlined bodies. *)
  let link_by_source_module : ((string * string * string), Module_link.t) Hashtbl.t =
    Hashtbl.create 16
  in
  let linked_bindings_by_local_name : (string, (binding_info * Module_link.t option) list) Hashtbl.t =
    Hashtbl.create 64
  in
  let add_linked_binding ~(binding : binding_info) ~(link : Module_link.t option) =
    let existing =
      match Hashtbl.find_opt linked_bindings_by_local_name binding.bi_local_name with
      | Some bindings -> bindings
      | None -> []
    in
    if not (List.exists (fun (binding', _) -> same_binding binding binding') existing) then
      Hashtbl.replace
        linked_bindings_by_local_name
        binding.bi_local_name
        ((binding, link) :: existing)
  in
  Hashtbl.iter (fun source_file scope_info ->
    List.iter (fun site ->
      let m = site.isi_provider.pi_module in
      let key = (source_file, site.isi_provider.pi_file, m) in
      if StringSet.mem m modules_needing_bodies then begin
        let link =
          match Hashtbl.find_opt link_by_source_module key with
          | Some link -> Some link
          | None ->
            (match Hashtbl.find_opt compiled_syntax_cache site.isi_provider.pi_file with
             | None -> None
             | Some xd_mod ->
               let link =
                 Module_link.build
                   ~importer_ic:ic
                   ~importer_source_file:source_file
                   ~provider_module:m
                   ~provider_file:site.isi_provider.pi_file
                   ~provider_syntax:xd_mod
               in
               List.iter add_fallback_binding_if_absent
                 (Module_link.binding_additions link);
               Hashtbl.replace link_by_source_module key link;
               Some link)
        in
        List.iter (fun binding ->
          add_linked_binding ~binding ~link
        ) site.isi_bindings
      end
    ) scope_info.si_direct_imports
  ) ic.ic_scope_info_by_source_file;

  let provider_by_local_prod_global : (string, rule * prod) Hashtbl.t = Hashtbl.create 2048 in
  Hashtbl.iter (fun _key link ->
    Hashtbl.iter (fun pn rp ->
      if not (Hashtbl.mem provider_by_local_prod_global pn) then
        Hashtbl.replace provider_by_local_prod_global pn rp
    ) (Module_link.provider_by_local_prod link)
  ) link_by_source_module;

  let provider_mvd_by_local_root_global : (string, metavardefn) Hashtbl.t = Hashtbl.create 512 in
  Hashtbl.iter (fun _key link ->
    Hashtbl.iter (fun root mvd ->
      if not (Hashtbl.mem provider_mvd_by_local_root_global root) then
        Hashtbl.replace provider_mvd_by_local_root_global root mvd
    ) (Module_link.provider_mvd_by_local_root link)
  ) link_by_source_module;
  let unique_linked_binding (local_name : string)
    : (binding_info * Module_link.t option) option =
    match Hashtbl.find_opt linked_bindings_by_local_name local_name with
    | Some [binding] -> Some binding
    | _ -> None
  in

  let rec rewrite_rel = function
    | RDC dc ->
      let dc_defns' =
        List.map (fun d ->
          (* Link bodies by the judgement-form production name, not by [d.d_name].
             In Ott, defn identifiers can repeat across defnclasses; the
             production name (dc_wrapper ^ d_name) is the unique identity. *)
          let local_prod = dc.dc_wrapper ^ d.d_name in
          match unique_linked_binding local_prod with
          | Some (binding, link) ->
            let m = binding.bi_provider.pi_module in
            let orig_prod = binding.bi_origin_name in
            (match Hashtbl.find_opt compiled_by_module m with
             | None -> d
             | Some idx ->
               match Hashtbl.find_opt idx.defns orig_prod with
               | None -> d
               | Some (src_dc_w, src_d_w, rules) ->
                 let rules' =
                   List.map (function
                     | PSR_Defncom es -> PSR_Defncom es
                     | PSR_Rule dr ->
                       let dr' =
                         {
                           dr with
                           drule_name =
                             rewrite_drule_name
                               ~src_defnclass_wrapper:src_dc_w
                               ~src_defn_wrapper:src_d_w
                               ~dst_defnclass_wrapper:dc.dc_wrapper
                               ~dst_defn_wrapper:d.d_wrapper
                               dr.drule_name;
                           drule_premises =
                             List.map (fun (hn, st) ->
                               (hn,
                                match link with
                                | None -> st
                                | Some l -> Module_link.rename_symterm l st)
                             ) dr.drule_premises;
                           drule_conclusion =
                             (match link with
                              | None -> dr.drule_conclusion
                             | Some l -> Module_link.rename_symterm l dr.drule_conclusion);
                         }
                       in
                       add_fallback_binding_if_absent
                         {
                           bi_local_name = dr'.drule_name;
                           bi_origin_name = dr.drule_name;
                           bi_provider = binding.bi_provider;
                           bi_visibility = Import_binding_internal;
                           bi_kinds = [];
                         };
                       PSR_Rule dr'
                   ) rules
                 in
                 { d with d_rules = rules' })
          | None -> d
        ) dc.dc_defns
      in
      RDC { dc with dc_defns = dc_defns' }
    | FDC fdc ->
      let fdc_fundefns' =
        List.map (fun fd ->
          (* Link bodies by function-form production name (pn_wrapper ^ fd_name). *)
          let local_prod = fd.fd_pn_wrapper ^ fd.fd_name in
          match unique_linked_binding local_prod with
          | Some (binding, link) ->
            let m = binding.bi_provider.pi_module in
            let orig_prod = binding.bi_origin_name in
            (match Hashtbl.find_opt compiled_by_module m with
             | None -> fd
             | Some idx ->
               match Hashtbl.find_opt idx.fundefns orig_prod with
               | None -> fd
               | Some fd_src ->
                 let clauses' =
                   List.map (fun fc ->
                     {
                       fc with
                       fc_lhs =
                         (match link with
                          | None -> fc.fc_lhs
                          | Some l -> Module_link.rename_symterm l fc.fc_lhs);
                       fc_rhs =
                         (match link with
                          | None -> fc.fc_rhs
                          | Some l -> Module_link.rename_symterm l fc.fc_rhs);
                     }
                   ) fd_src.fd_clauses
                 in
                 { fd with fd_clauses = clauses' })
          | None -> fd
        ) fdc.fdc_fundefns
      in
      FDC { fdc with fdc_fundefns = fdc_fundefns' }
  in

  let relations' = List.map rewrite_rel sd.relations in

  (* Ensure TeX printing can look up every production referenced in inlined
     bodies. In particular, imported rule bodies can reference productions from
     grammar fragments (e.g. user-defined [formula]) that were not needed for
     typechecking (because imported bodies were stripped), and therefore may be
     missing from [sd.syntax].

     We treat this as part of the TeX “link” step: patch the typed syntax with
     any missing productions, sourcing them from the per-module syntaxdefs we
     already compiled while extracting bodies. *)
  let referenced_prodnames = prodnames_of_relations relations' in
  let available_prodnames = prodname_set_of_syntax syntax0 in
  let missing_prodnames = StringSet.diff referenced_prodnames available_prodnames in

  let syntax' =
    if StringSet.is_empty missing_prodnames then sd.syntax
    else
      StringSet.fold (fun pn xd_acc ->
        match Hashtbl.find_opt provider_by_local_prod_global pn with
        | None -> xd_acc
        | Some (r, p) -> merge_prod_into_syntax xd_acc ~src_rule:r ~src_prod:p
      ) missing_prodnames sd.syntax
  in

  (* Ensure TeX printing cannot fail due to missing metavariable declarations
     introduced by newly merged productions. *)
  let syntax' =
    let declared_mvr : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    List.iter (fun mvd ->
      List.iter (fun (mvr, _hs) -> Hashtbl.replace declared_mvr mvr ()) mvd.mvd_names
    ) syntax'.xd_mds;

    let referenced_mvr : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    let add_mvr (mvr : string) = Hashtbl.replace referenced_mvr mvr () in
    let rec add_mse (m : mse) : unit =
      match m with
      | MetaVarExp (mvr, _suff) -> add_mvr mvr
      | MetaVarListExp ((mvr, _suff), _b) -> add_mvr mvr
      | NonTermExp _ | NonTermListExp _ -> ()
      | Aux (_f, _nt) -> ()
      | AuxList (_f, _nt, _b) -> ()
      | Union (m1, m2) -> add_mse m1; add_mse m2
      | Empty -> ()
    in
    let add_bindspec (bs : bindspec) : unit =
      match bs with
      | Bind (_l, m, _nt) -> add_mse m
      | AuxFnDef (_l, _f, m) -> add_mse m
      | NamesEqual (_l, m1, m2) -> add_mse m1; add_mse m2
      | NamesDistinct (_l, m1, m2) -> add_mse m1; add_mse m2
      | AllNamesDistinct (_l, m) -> add_mse m
    in
    let rec add_element (e : element) : unit =
      match e with
      | Lang_metavar (mvrp, (mvr, _suff)) ->
        add_mvr mvrp;
        add_mvr mvr
      | Lang_nonterm _ | Lang_terminal _ -> ()
      | Lang_option es -> List.iter add_element es
      | Lang_sugaroption _ -> ()
      | Lang_list elb -> List.iter add_element elb.elb_es
    in
    List.iter (fun r ->
      List.iter (fun p ->
        List.iter add_element p.prod_es;
        List.iter add_bindspec p.prod_bs
      ) r.rule_ps
    ) syntax'.xd_rs;

    let existing_mvd_names : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    List.iter (fun mvd -> Hashtbl.replace existing_mvd_names mvd.mvd_name ()) syntax'.xd_mds;
    let to_add : metavardefn list ref = ref [] in
    Hashtbl.iter (fun mvr () ->
      if not (Hashtbl.mem declared_mvr mvr) then
        match Hashtbl.find_opt provider_mvd_by_local_root_global mvr with
        | None -> ()
        | Some mvd ->
          if Hashtbl.mem existing_mvd_names mvd.mvd_name then ()
          else begin
            Hashtbl.replace existing_mvd_names mvd.mvd_name ();
            to_add := mvd :: !to_add
          end
    ) referenced_mvr;
    let syntax'' =
      if !to_add = [] then syntax' else { syntax' with xd_mds = syntax'.xd_mds @ List.rev !to_add }
    in
    (* Postcondition: every referenced metavar root in grammar productions must
       have a corresponding declaration in [xd_mds], otherwise TeX printing can
       raise [Not_found]. *)
    let declared_mvr'' : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    List.iter (fun mvd ->
      List.iter (fun (mvr, _hs) -> Hashtbl.replace declared_mvr'' mvr ()) mvd.mvd_names
    ) syntax''.xd_mds;
    let missing = ref [] in
    Hashtbl.iter (fun mvr () ->
      if not (Hashtbl.mem declared_mvr'' mvr) then missing := mvr :: !missing
    ) referenced_mvr;
    (match !missing with
     | [] -> syntax''
     | ms ->
       let ms = List.sort_uniq String.compare ms in
       let shown =
         let rec take n acc = function
           | [] -> List.rev acc
           | _ when n = 0 -> List.rev acc
           | x :: xs -> take (n - 1) (x :: acc) xs
         in
         take 20 [] ms
       in
       Auxl.error None
         ("Internal error (TeX link): missing metavariable declarations for:\n  "
          ^ String.concat "\n  " shown
          ^ (if List.length ms > List.length shown then "\n  ..." else "")
          ^ "\n"))
  in

  { sd with syntax = { syntax' with xd_imports = ic }; relations = relations' }
