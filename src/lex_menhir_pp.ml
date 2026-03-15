(**************************************************************************)
(*                    ocamllex/menhir partial backend                     *)
(*                                                                        *)
(*                     Peter Sewell, University of Cambridge              *)
(*                                                                        *)
(*  Copyright 2017                                                        *)
(**************************************************************************)

(**************************************************************************)
(*                    ocamllex/ocamlyacc backend                          *)
(*                                                                        *)
(*                     Viktor Vafeiadis, MPI-SWS                          *)
(*                                                                        *)
(*  Copyright 2011                                                        *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*  3. The names of the authors may not be used to endorse or promote     *)
(*  products derived from this software without specific prior written    *)
(*  permission.                                                           *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS    *)
(*  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE    *)
(*  ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY       *)
(*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    *)
(*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE     *)
(*  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS         *)
(*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER  *)
(*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR       *)
(*  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN   *)
(*  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                         *)
(**************************************************************************)

(*

This is a partial Ott backend to produce, given an Ott input file,
basic source files for the menhir/ocamllex parser and lexer generators,
and for ocaml pretty printers of raw and source terms. 

In general this is impossible: 
- fundamentally, because an Ott grammar can be an arbitrary
   context-free grammar, whereas menhir supports LR grammars; and
- practically, because one may also want to hand-tune the grammar or
   lexer, e.g. for decent parse error messages/recovery

The normal goal for Ott development has been to produce files that do
not need hand-editing, so that the Ott source remains the principal
source.  Here we may not be able to achieve that: we might aim instead
to produce menhir/ocamllex source files that can be hand-edited, to
take some of the tedious work out of producing a parser and lexer;
we'll see.

Along with this, Ott now supports maintaining both a context-free and LR
grammar in sync, by writing an LR Ott grammar but including
annotations saying that some rules should be quotiented together in
the context-free grammar.

The implementation is partial in several respects.  Currently it:
- does not support subrules
   (it should only take the maximal elements from the subrule order)
- does not support {{ order }} homs
- does not support ocaml type or variable name homs
- does not check or enforce ocaml naming conventions, eg capitalisation
- requires an explicit hom for syntactic sugar productions

There are some minimal working examples in tests/menhir_tests. In the Ott source:
- metavar definitions should have ocaml and ocamllex homs, to specify
   their OCaml type and the ocamllex regexps.
- grammar rules that are start symbols should have "menhir-start" homs

Command-line usage should specify single .ml, .mll, and .mly output
files, in a single run of Ott (as the files have to refer to each
other).  The output also generates a simple pretty printer for the ast. 

There was a previous attempt at an ocamllex/ocamlyacc backend, by
Viktor Vafeiadis in 2011, but that code seemed to be very partial.
This implementation repurposes a little of that infrastructure. 

*)


(* for the generated lexer, metavar definitions should either have an ocamllex hom (specifying how they should be lexed) or an ocamllex-remove hom (specifying that a constructor of the token type should be generated, but without a lexer rule). *)


open Types;;

module Source_metavar_set =
  Set.Make (struct
    type t = string * string
    let compare = compare
  end)

(* which metavars, rules and productions to include *)
let suppress_metavar yo mvd =
  mvd.mvd_phantom  (* was: mvd.mvd_indexvar - now only suppress phantom metavars *)

let contains_list p = 
  List.exists (function e -> match e with Lang_list _ -> true | _ -> false  )  p.prod_es

let suppress_prod yo p = 
  let suppressed_category = 
    StringSet.exists (fun x -> List.mem x yo.ppm_suppressed_categories)
      p.prod_categories in

 (* contains_list p (* just for now... *)
   ||*) suppressed_category || ((*not(yo.ppm_show_meta) &&*) p.prod_meta && not(p.prod_sugar))

let has_hom hn hs = match Auxl.hom_spec_for_hom_name hn hs with Some _ -> true | None -> false

(* identify which nonterminal-root rules should be suppressed in the generated menhir rule set *)
let suppress_rule yo r = 
  let suppressed_ntr = 
    List.mem r.rule_ntr_name yo.ppm_suppressed_ntrs
  in
  let menhir_suppress = has_hom "menhir-suppress" r.rule_homs in
  (* Check if this nonterminal is non-maximal in the subrule hierarchy *)
  let subrule_non_maximal =
    match yo.syntaxdefn with
    | None -> false (* If syntaxdefn is not available, don't suppress anything based on subrules *)
    | Some xd -> List.exists (function sr -> sr.sr_lower = r.rule_ntr_name) xd.xd_srs
  in
  [] = r.rule_ps (*List.filter (function p -> not (contains_list p)) r.rule_ps *)
(*|| suppressed_ntr || (not(yo.ppm_show_meta) && r.rule_semi_meta) || r.rule_meta*)
  || suppressed_ntr || r.rule_semi_meta || subrule_non_maximal || menhir_suppress

let source_file_of_loc_opt (loc : loc) : string option =
  match loc with
  | [] -> None
  | _ -> Some (Import_naming.source_file_of_loc ~default:"" loc)

let with_loc_source (loc : loc) (f : unit -> 'a) : 'a =
  Import.with_current_source_file (source_file_of_loc_opt loc) f

let with_menhir_syntaxdefn yo xd (f : unit -> 'a) : 'a =
  let saved = yo.syntaxdefn in
  yo.syntaxdefn <- Some xd;
  try
    let result = f () in
    yo.syntaxdefn <- saved;
    result
  with exn ->
    yo.syntaxdefn <- saved;
    raise exn

let canonical_source_file source_file =
  if source_file = "" then
    source_file
  else
    let absolute_source_file =
      if Filename.is_relative source_file then
        Filename.concat (Sys.getcwd ()) source_file
      else
        source_file
    in
    try
      Unix.realpath absolute_source_file
    with Unix.Unix_error _ ->
      absolute_source_file

let local_structure ic structure =
  List.filter (fun (fn, _) -> not (Import.is_imported_file ic fn)) structure

type local_backend_view = {
  lbv_structure : structure;
  lbv_rules : rule list;
}

type token_deps = {
  td_terminals : StringSet.t;
  td_metavars : Source_metavar_set.t;
}

type imported_printer_dep = {
  ip_provider : provider_info;
  ip_origin_name : string;
}

type resolved_nonterm_ref = {
  rnr_actual_ntr : nontermroot;
  rnr_runtime_check : string option;
  rnr_rule : rule;
  rnr_imported_dep : imported_printer_dep option;
}

type menhir_imported_use = {
  miu_use_loc : loc;
  miu_requested_ntr : nontermroot;
  miu_actual_ntr : nontermroot;
  miu_actual_rule : rule;
}

type menhir_rule_summary = {
  mrs_token_deps : token_deps;
  mrs_direct_providers : provider_info list;
  mrs_imported_pp_deps : imported_printer_dep list;
  mrs_imported_uses : menhir_imported_use list;
}

let empty_token_deps = {
  td_terminals = StringSet.empty;
  td_metavars = Source_metavar_set.empty;
}

let union_token_deps d1 d2 = {
  td_terminals = StringSet.union d1.td_terminals d2.td_terminals;
  td_metavars = Source_metavar_set.union d1.td_metavars d2.td_metavars;
}

let local_root_sets structure =
  List.fold_left (fun (mvrs, ntrs) (_fn, entry) ->
    match entry with
    | Struct_md mvr ->
        (StringSet.add mvr mvrs, ntrs)
    | Struct_rs roots ->
        (mvrs, List.fold_left (fun acc root -> StringSet.add root acc) ntrs roots)
    | _ -> (mvrs, ntrs)
  ) (StringSet.empty, StringSet.empty) structure

let quotient_with_target r =
  match Auxl.hom_spec_for_hom_name "quotient-with" r.rule_homs with
  | Some [Hom_string s] -> Some s
  | Some _ -> raise (Failure ("unexpected quotient-with hom for " ^ r.rule_ntr_name))
  | None -> None

let local_rules_of_structure xd structure =
  let _, initial_local_ntrs = local_root_sets structure in
  let rec close_over_quotient_with local_ntrs =
    let expanded =
      List.fold_left (fun acc r ->
        match quotient_with_target r with
        | Some target when StringSet.mem target acc -> StringSet.add r.rule_ntr_name acc
        | _ -> acc
      ) local_ntrs xd.xd_rs
    in
    if StringSet.equal expanded local_ntrs
    then local_ntrs
    else close_over_quotient_with expanded
  in
  let local_ntrs = close_over_quotient_with initial_local_ntrs in
  List.filter (fun r -> StringSet.mem r.rule_ntr_name local_ntrs) xd.xd_rs

let structure_for_sources structure source_files =
  let local_sources =
    List.fold_left
      (fun acc source_file -> StringSet.add (canonical_source_file source_file) acc)
      StringSet.empty
      source_files
  in
  let structure =
    List.filter (fun (fn, _) ->
      StringSet.mem (canonical_source_file fn) local_sources
    ) structure
  in
  structure

let local_backend_view ?source_files xd structure =
  let structure =
    match source_files with
    | Some source_files ->
        structure_for_sources structure source_files
    | None when Import.direct_providers xd.xd_imports = [] ->
        structure
    | None ->
        local_structure xd.xd_imports structure
  in
  let rules =
    match source_files with
    | None when Import.direct_providers xd.xd_imports = [] ->
        xd.xd_rs
    | _ ->
        local_rules_of_structure xd structure
  in
  { lbv_structure = structure; lbv_rules = rules }

let actual_nonterm_and_runtime_check xd ntr =
  match List.find_opt (function sr -> sr.sr_lower = ntr) xd.xd_srs with
  | Some sr -> (sr.sr_top, Some ("is_" ^ ntr ^ "_of_" ^ sr.sr_top))
  | None -> (ntr, None)

let menhir_rule_source r =
  canonical_source_file (Import_naming.source_file_of_loc ~default:"" r.rule_loc)

let resolve_imported_nonterm_dep xd ~source_file ntr actual_rule =
  let provider_file = menhir_rule_source actual_rule in
  match Import.lookup_binding_in_source xd.xd_imports ~source_file ntr with
  | Some binding when binding.bi_provider.pi_file = provider_file ->
      {
        ip_provider = binding.bi_provider;
        ip_origin_name = binding.bi_origin_name;
      }
  | Some binding ->
      Auxl.error (Some actual_rule.rule_loc)
        ("menhir output: imported nonterminal " ^ ntr
         ^ " resolves to " ^ binding.bi_provider.pi_file
         ^ " but rule comes from " ^ provider_file ^ "\n")
  | None ->
      Auxl.error (Some actual_rule.rule_loc)
        ("menhir output: missing direct import binding for " ^ ntr ^ "\n")

let resolved_nonterm_ref xd ~source_file ntr =
  let actual_ntr, runtime_check = actual_nonterm_and_runtime_check xd ntr in
  let actual_rule = Auxl.rule_of_ntr_nonprimary xd actual_ntr in
  let imported_dep =
    if Import.is_imported_loc xd.xd_imports actual_rule.rule_loc then
      Some (resolve_imported_nonterm_dep xd ~source_file ntr actual_rule)
    else
      None
  in
  {
    rnr_actual_ntr = actual_ntr;
    rnr_runtime_check = runtime_check;
    rnr_rule = actual_rule;
    rnr_imported_dep = imported_dep;
  }

let menhir_rule_key r =
  menhir_rule_source r ^ ":" ^ r.rule_ntr_name

let menhir_rule_dependencies yo xd r =
  if suppress_rule yo r then
    []
  else
    let deps_rev = ref [] in
    let seen = Hashtbl.create 8 in
    let add_rule dep_rule =
      let key = menhir_rule_key dep_rule in
      if not (Hashtbl.mem seen key) then begin
        Hashtbl.replace seen key ();
        deps_rev := dep_rule :: !deps_rev
      end
    in
    let rec visit_element = function
      | Lang_nonterm (ntr, _) ->
          let actual_ntr, _ = actual_nonterm_and_runtime_check xd ntr in
          let dep_rule = Auxl.rule_of_ntr_nonprimary xd actual_ntr in
          if not (suppress_rule yo dep_rule) then
            add_rule dep_rule
      | Lang_option es -> List.iter visit_element es
      | Lang_list elb -> List.iter visit_element elb.elb_es
      | Lang_metavar _
      | Lang_terminal _
      | Lang_sugaroption _ -> ()
    in
    List.iter (fun p ->
      if not (suppress_prod yo p) then
        List.iter visit_element p.prod_es
    ) r.rule_ps;
    List.rev !deps_rev

let close_rule_set roots all_rules deps_of =
  let seen = Hashtbl.create 32 in
  let rec visit_rule r =
    let key = menhir_rule_key r in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.replace seen key ();
      List.iter visit_rule (deps_of r)
    end
  in
  List.iter visit_rule roots;
  List.filter (fun r -> Hashtbl.mem seen (menhir_rule_key r)) all_rules

let local_rule_sources local_view =
  let from_structure =
    List.fold_left (fun acc (fn, _) ->
      StringSet.add (canonical_source_file fn) acc
    ) StringSet.empty local_view.lbv_structure
  in
  List.fold_left (fun acc r ->
    StringSet.add (menhir_rule_source r) acc
  ) from_structure local_view.lbv_rules

let menhir_local_rule_dependencies yo xd local_sources r =
  List.filter
    (fun dep -> StringSet.mem (menhir_rule_source dep) local_sources)
    (menhir_rule_dependencies yo xd r)

let rec iter_nonterms_in_element f e =
  match e with
  | Lang_nonterm (ntr, _) -> f ntr
  | Lang_option es -> List.iter (iter_nonterms_in_element f) es
  | Lang_list elb -> List.iter (iter_nonterms_in_element f) elb.elb_es
  | Lang_metavar _
  | Lang_terminal _
  | Lang_sugaroption _ -> ()

let remember_menhir_provider yo seen providers_rev provider =
  let key = provider.pi_file in
  match Hashtbl.find_opt seen key with
  | None ->
      Hashtbl.replace seen key provider;
      providers_rev := provider :: !providers_rev
  | Some existing ->
      if Import_backend.module_id (Menhir yo) existing
         <> Import_backend.module_id (Menhir yo) provider then
        Auxl.error None
          ("menhir output: inconsistent OCaml paths for imported provider "
           ^ provider.pi_file ^ "\n")

let add_imported_pp_dep seen deps_rev dep =
  let key = (dep.ip_provider, dep.ip_origin_name) in
  if not (Hashtbl.mem seen key) then begin
    Hashtbl.replace seen key ();
    deps_rev := dep :: !deps_rev
  end

let validated_imported_pp_dep actual_rule imported_dep =
  if Auxl.hom_spec_for_hom_name "pp-params" actual_rule.rule_homs <> None then
    Auxl.error (Some actual_rule.rule_loc)
      ("menhir output: imported pretty-printer dependencies do not support pp-params for "
       ^ imported_dep.ip_origin_name ^ "\n");
  imported_dep

let lex_comment_string_of_rule r =
  match Auxl.hom_spec_for_hom_name "lex-comment" r.rule_homs with
  | None -> None
  | Some [Hom_string s] -> Some s
  | Some _ ->
      Auxl.error (Some r.rule_loc)
        ("ocamllex output: lex-comment hom for " ^ r.rule_ntr_name
         ^ " must be a single string\n")

let comment_start_of_local_rules yo rs =
  let comment_start = ref None in
  List.iter (fun r ->
    with_loc_source r.rule_loc (fun () ->
      if not (suppress_rule yo r) then
        match lex_comment_string_of_rule r with
        | None -> ()
        | Some marker ->
            match !comment_start with
            | None -> comment_start := Some marker
            | Some existing when existing = marker -> ()
            | Some existing ->
                Auxl.error (Some r.rule_loc)
                  ("ocamllex output: conflicting local lex-comment markers "
                   ^ existing ^ " and " ^ marker ^ "\n"))
  ) rs;
  match !comment_start with
  | Some marker -> marker
  | None -> "//"
let local_menhir_embeds structure =
  Auxl.option_map
    (function
      | (_fn, Struct_embed embed) -> Some embed
      | _ -> None)
    structure

let pp_local_menhir_embeds fd m xd lookup structure =
  (* Raw imported Menhir embeds are intentionally ignored. Importers that need
     precedence or other declaration-phase control must restate it locally
     against their own importer-visible token names. *)
  Embed_pp.pp_embeds fd m xd lookup (local_menhir_embeds structure)

(* tokens arise from terminals and metavardefns *)
type token_kind = 
  | TK_terminal 
  | TK_metavar of string (* ocamllex hom *) * string option (* ocaml hom, for type*) * string option (* ocamllex_of_string hom, for function to convert from string to ocaml type*)
(* or other way round?*)

type token_subject =
  | TS_terminal of string
  | TS_metavar of string * string

type token_entry = {
  te_name : string;
  te_subject : token_subject;
  te_kind : token_kind;
  te_var_name : string option;
  (* Compatible source-local metavars can share one emitted token. *)
  te_metavar_aliases : Source_metavar_set.t;
}

type token_data = token_entry list


(* find the token associated with a terminal or metavarroot *) 
let token_of_terminal ts t0 = 
  try
    (List.find
       (function
         | { te_name; te_subject = TS_terminal t; te_kind = TK_terminal }
           when t = t0 -> true
         | _ -> false)
       ts).te_name
  with Not_found -> raise (Failure ("token_of_terminal \""^t0^"\" not found"))

let token_name_basis_of_metavarroot_in_source ic ~source_file mvr =
  let source_file = canonical_source_file source_file in
  match Import.lookup_binding_in_source ic ~source_file mvr with
  | Some binding ->
      (match Import_naming.surface_of_internal_token binding.bi_local_name with
       | Some s -> s
       | None -> binding.bi_local_name)
  | None ->
      (match Import_naming.surface_of_internal_token mvr with
       | Some s -> s
       | None -> mvr)

let source_metavar_alias ~source_file mvr =
  (canonical_source_file source_file, mvr)

let source_metavar_alias_set ~source_file mvr =
  Source_metavar_set.singleton (source_metavar_alias ~source_file mvr)

let token_of_metavarroot ts ~source_file mvr =
  let source_alias = source_metavar_alias ~source_file mvr in
  try
    (List.find
       (function
         | { te_kind = TK_metavar _;
             te_metavar_aliases; _ } ->
             Source_metavar_set.mem source_alias te_metavar_aliases
         | _ -> false)
       ts).te_name
  with Not_found ->
    raise (Failure ("token_of_metavarroot \"" ^ mvr ^ "\" not found"))


let rec token_deps_of_element source_file e =
  match e with
  | Lang_terminal t ->
      { empty_token_deps with td_terminals = StringSet.singleton t }
  | Lang_metavar (mvr, _) ->
      { empty_token_deps with
        td_metavars = source_metavar_alias_set ~source_file mvr }
  | Lang_list elb ->
      let deps =
        List.fold_left
          (fun acc e -> union_token_deps acc (token_deps_of_element source_file e))
          empty_token_deps elb.elb_es
      in
      (match elb.elb_tmo with
       | None -> deps
       | Some t ->
           { deps with td_terminals = StringSet.add t deps.td_terminals })
  | _ ->
      empty_token_deps

let token_deps_of_prod yo source_file p =
  if suppress_prod yo p then
    empty_token_deps
  else
    List.fold_left
      (fun acc e -> union_token_deps acc (token_deps_of_element source_file e))
      empty_token_deps p.prod_es

let token_deps_of_rules yo rs =
  List.fold_left
    (fun acc r ->
       if suppress_rule yo r then
         acc
       else
         let source_file = menhir_rule_source r in
         List.fold_left
           (fun acc p -> union_token_deps acc (token_deps_of_prod yo source_file p))
           acc r.rule_ps)
    empty_token_deps rs

let menhir_rule_summary yo xd rs =
  let seen_providers = Hashtbl.create 16 in
  let providers_rev = ref [] in
  let seen_pp_deps = Hashtbl.create 16 in
  let deps_rev = ref [] in
  let imported_uses_rev = ref [] in
  List.iter (fun r ->
    let source_file = menhir_rule_source r in
    if source_file <> "" && not (suppress_rule yo r) then
      List.iter (fun p ->
        if not (suppress_prod yo p) then
          List.iter
            (iter_nonterms_in_element (fun ntr ->
               let resolved_ref = resolved_nonterm_ref xd ~source_file ntr in
               match resolved_ref.rnr_imported_dep with
               | None -> ()
               | Some imported_dep ->
                   remember_menhir_provider yo seen_providers providers_rev
                     imported_dep.ip_provider;
                   imported_uses_rev :=
                     {
                       miu_use_loc = p.prod_loc;
                       miu_requested_ntr = ntr;
                       miu_actual_ntr = resolved_ref.rnr_actual_ntr;
                       miu_actual_rule = resolved_ref.rnr_rule;
                     }
                     :: !imported_uses_rev;
                   if not p.prod_sugar then
                     add_imported_pp_dep seen_pp_deps deps_rev
                       (validated_imported_pp_dep resolved_ref.rnr_rule
                          imported_dep)
            ))
            p.prod_es
      ) r.rule_ps
  ) rs;
  {
    mrs_token_deps = token_deps_of_rules yo rs;
    mrs_direct_providers = List.rev !providers_rev;
    mrs_imported_pp_deps = List.rev !deps_rev;
    mrs_imported_uses = List.rev !imported_uses_rev;
  }

let menhir_lib_rules_of_local_view yo xd local_view =
  let public_rules =
    List.filter
      (fun r -> not (suppress_rule yo r) && has_hom "menhir-public" r.rule_homs)
      local_view.lbv_rules
  in
  match public_rules with
  | [] -> None
  | _ ->
      let local_sources = local_rule_sources local_view in
      Some (
        close_rule_set public_rules local_view.lbv_rules
          (menhir_local_rule_dependencies yo xd local_sources))
 
(* make ocamllex token names by uniformly replacing non-alphabetic
characters and smashing to uppercase, then uniqueify (tokens from
terminals and from metavars) afterwards by appending a number if
necessary, keeping the result in a lookup table *)

(* token name munging *)
let token_escape_map = 
  [
   ('!', "BANG");
   ('\"', "QUOTE");
    ('#', "HASH");
    ('$', "DOLLAR");
    ('%', "PERCENT");
    ('&', "AMPERSAND");
    ('\'', "APOSTROPHE");
    ('(', "LPAREN");
    (')', "RPAREN");
    ('*', "STAR");
    ('+', "PLUS");
    (',', "COMMA");
    ('-', "MINUS");
    ('.', "DOT");
    ('/', "SLASH");
    ('0', "ZERO");
    ('1', "ONE");
    ('2', "TWO");
    ('3', "THREE");
    ('4', "FOUR");
    ('5', "FIVE");
    ('6', "SIX");
    ('7', "SEVEN");
    ('8', "EIGHT");
    ('9', "NINE");
    (':', "COLON");
    (';', "SEMICOLON");
    ('<', "LT");
    ('=', "EQ");
    ('>', "GT");
    ('?', "QUESTIONMARK");
    ('@', "AT");
    ('[', "LBRACK");
    ('\\', "BACKSLASH");
    (']', "RBRACK");
    ('^', "CARET");
    ('_', "UNDERSCORE");
    ('`', "BACKTICK");
    ('{', "LBRACE");
    ('|', "BAR");
    ('}', "RBRACE");
    ('~', "TILDE")
  ]

let token_name_char (c:char) : (string * bool (*symbolic?*)) =
  if c >='A' && c <= 'Z' then (String.make 1 c, false)
  else if c >='a' && c <= 'z' then (String.make 1 (Char.chr (Char.code c - 32)), false)
  else 
    try (List.assoc c token_escape_map, true) with
    | Not_found -> raise (Failure "token_of_terminal given non-ASCII character")

let rec token_name_char_list (cs:char list) : (string * bool (*front is symbolic*)) =
  match cs with
  | [c] -> token_name_char c
  | c::cs -> 
      (match (token_name_char c, token_name_char_list cs) with
      | ((s,false), (s',false)) -> (s ^ s',      false)
      | ((s,false), (s',true )) -> (s ^ "_" ^ s',false)
      | ((s,true),  (s',false)) -> (s ^ "_" ^ s', true)
      | ((s,true),  (s',true )) -> (s ^ "_" ^ s', true))
  | [] -> raise (Failure "token_escape_char_list given empty list")

let token_name_of t = 
  let cs = Auxl.char_list_of_string t in
  let (s, _) = token_name_char_list cs in
  s

let token_entries_compatible entry1 entry2 =
  match entry1.te_subject, entry2.te_subject, entry1.te_kind, entry2.te_kind with
  | TS_terminal t1, TS_terminal t2, TK_terminal, TK_terminal -> t1 = t2
  | TS_metavar _, TS_metavar _, TK_metavar (ty1, lex1, conv1),
    TK_metavar (ty2, lex2, conv2) ->
      ty1 = ty2 && lex1 = lex2 && conv1 = conv2
  | _ -> false

let merge_token_entries existing entry =
  match existing.te_kind, entry.te_kind with
  | TK_metavar _, TK_metavar _ ->
      { existing with
        te_metavar_aliases =
          Source_metavar_set.union
            existing.te_metavar_aliases entry.te_metavar_aliases }
  | TK_terminal, TK_terminal ->
      existing
  | _ ->
      existing

let dedup_token_entries_with compatible entries =
  let string_of_token_subject = function
    | TS_terminal t -> "terminal:" ^ t
    | TS_metavar (source_file, mvr) -> "metavar:" ^ source_file ^ ":" ^ mvr
  in
  let string_of_token_kind = function
    | TK_terminal -> "terminal"
    | TK_metavar (ty, lex_hom, conv_hom) ->
        "metavar<" ^ ty ^ ">"
        ^ (match lex_hom with None -> "" | Some _ -> ":lex")
        ^ (match conv_hom with None -> "" | Some _ -> ":conv")
  in
  let by_name = Hashtbl.create 32 in
  let deduped_rev = ref [] in
  List.iter (fun entry ->
    match Hashtbl.find_opt by_name entry.te_name with
    | None ->
        Hashtbl.replace by_name entry.te_name entry;
        deduped_rev := entry :: !deduped_rev
    | Some existing when compatible existing entry ->
        Hashtbl.replace by_name entry.te_name (merge_token_entries existing entry)
    | Some _ ->
        Auxl.error None
          ("menhir output: incompatible token name collision for "
           ^ entry.te_name ^ " ("
           ^ string_of_token_subject entry.te_subject ^ ", "
           ^ string_of_token_kind entry.te_kind ^ ")\n")
  ) entries;
  List.rev_map
    (fun entry -> Hashtbl.find by_name entry.te_name)
    !deduped_rev

let dedup_token_entries entries =
  dedup_token_entries_with token_entries_compatible entries

(* materialize exact token entries for the requested dependencies *)
let token_entries_of_deps yo xd deps : token_data =
  let m = Menhir yo in
  let ts_terminals =
    List.map
      (fun t ->
         {
           te_name = token_name_of t;
           te_subject = TS_terminal t;
           te_kind = TK_terminal;
           te_var_name = None;
           te_metavar_aliases = Source_metavar_set.empty;
         })
      (StringSet.elements deps.td_terminals)
  in
  let ts_metavars =
    Auxl.option_map
      (fun (source_file, mvr) ->
         let mvd = Auxl.mvd_of_mvr_nonprimary xd mvr in
         with_loc_source mvd.mvd_loc (fun () ->
           match suppress_metavar yo mvd with
           | true -> None
           | false ->
               let ocaml_type =
                 (try
	            let hs = List.assoc "ocaml" mvd.mvd_rep in
	            Grammar_pp.pp_hom_spec_in_type_context m xd hs
                 with Not_found ->
                   Auxl.error (Some mvd.mvd_loc)
                     ("ocamllex output: undefined ocaml hom for "
                      ^ mvd.mvd_name ^ "\n")) in
               let ocamllex_hom_opt =
                 (try
	            let hs = List.assoc "ocamllex" mvd.mvd_rep in
	            Some (Grammar_pp.pp_hom_spec m xd hs)
                 with Not_found -> None) in
               let ocamllex_of_string_hom_opt =
                 (try
	            let hs = List.assoc "ocamllex-of-string" mvd.mvd_rep in
	            Some (Grammar_pp.pp_hom_spec m xd hs)
                 with Not_found -> None) in
               let ocamllex_remove_hom =
                 (try
	            let _ = List.assoc "ocamllex-remove" mvd.mvd_rep in
	            true
                 with Not_found -> false) in
               let token_kind =
                 match ocamllex_hom_opt, ocamllex_remove_hom with
                 | Some ocamllex_hom, false ->
                     TK_metavar
                       (ocaml_type, Some ocamllex_hom, ocamllex_of_string_hom_opt)
                 | None, false ->
                     TK_metavar
                       (ocaml_type, None, ocamllex_of_string_hom_opt)
                 | Some _, true ->
                     Auxl.error (Some mvd.mvd_loc)
                       ("ocamllex output: both ocamllex and ocamllex-remove hom for "
                        ^ mvd.mvd_name ^ "\n")
                 | None, true ->
                     TK_metavar
                       (ocaml_type, None, ocamllex_of_string_hom_opt)
               in
               let var_name =
                 token_name_basis_of_metavarroot_in_source xd.xd_imports
                   ~source_file mvr
               in
               Some {
                 te_name = token_name_of var_name;
                 te_subject = TS_metavar (source_file, mvr);
                 te_kind = token_kind;
                 te_var_name = Some var_name;
                 te_metavar_aliases = source_metavar_alias_set ~source_file mvr;
               }))
      (Source_metavar_set.elements deps.td_metavars)
  in
  dedup_token_entries
    (List.stable_sort
       (fun entry1 entry2 ->
          match entry1.te_kind, entry2.te_kind with
          | TK_terminal, TK_metavar _ -> -1
          | TK_metavar _, TK_terminal -> 1
          | _, _ ->
              compare (String.length entry2.te_name) (String.length entry1.te_name))
       (ts_terminals @ ts_metavars))

type menhir_provider_lib_summary = {
  mpls_tokens : token_data;
  mpls_direct_providers : provider_info list;
}

let raw_item_source_file ~default raw_item =
  canonical_source_file
    (Import_naming.source_file_of_loc ~default (Import.loc_of_item raw_item))

let group_raw_items_by_source_file ~default raw_items =
  let buckets = Hashtbl.create 8 in
  let source_files_rev = ref [] in
  List.iter (fun raw_item ->
    let source_file = raw_item_source_file ~default raw_item in
    match Hashtbl.find_opt buckets source_file with
    | Some items_rev ->
        Hashtbl.replace buckets source_file (raw_item :: items_rev)
    | None ->
        Hashtbl.add buckets source_file [raw_item];
        source_files_rev := source_file :: !source_files_rev
  ) raw_items;
  let source_files = List.rev !source_files_rev in
  let items_per_file =
    List.map (fun source_file ->
      List.rev (Hashtbl.find buckets source_file)
    ) source_files
  in
  (source_files, items_per_file)

(* Menhir generation itself does not participate in the target-based hom check.
   The parser backend consumes OCaml semantic actions and validates Menhir
   annotations separately, so provider-local views should follow the same
   discipline as the real Menhir pipeline: require only OCaml action homs here.
   Re-checking providers under target "menhir" would incorrectly reject
   OCaml-only sugar/meta productions that the main backend accepts. *)
let menhir_provider_view_targets =
  ["ocaml"]

let menhir_provider_typed_view yo root_import_ctx provider =
  match Import.get_loaded_module_items provider.pi_file with
  | None ->
      Auxl.error None
        ("menhir output: missing cached provider-local view for imported provider "
         ^ provider.pi_file ^ "\n")
  | Some raw_items ->
      let raw_items =
        List.filter (function Raw_item_import _ -> false | _ -> true) raw_items
      in
      let source_files, items_per_file =
        group_raw_items_by_source_file ~default:provider.pi_file raw_items
      in
      let source_files, items_per_file =
        match source_files, items_per_file with
        | [], [] -> ([provider.pi_file], [raw_items])
        | _ -> (source_files, items_per_file)
      in
      let xd, structure, _ =
        Grammar_typecheck.check_and_disambiguate
          (Menhir yo) false false menhir_provider_view_targets
          source_files false true root_import_ctx items_per_file
      in
      (xd, structure)

let menhir_provider_lib_summary yo xd cache provider =
  match Hashtbl.find_opt cache provider.pi_file with
  | Some lib_summary -> lib_summary
  | None ->
      let lib_summary =
        let provider_xd, provider_structure =
          menhir_provider_typed_view yo xd.xd_imports provider
        in
        with_menhir_syntaxdefn yo provider_xd (fun () ->
          let local_view =
            local_backend_view
              ~source_files:[provider.pi_file]
              provider_xd provider_structure
          in
          match menhir_lib_rules_of_local_view yo provider_xd local_view with
          | None ->
              {
                mpls_tokens = [];
                mpls_direct_providers = [];
              }
          | Some lib_rules ->
              let lib_summary = menhir_rule_summary yo provider_xd lib_rules in
              {
                mpls_tokens =
                  token_entries_of_deps yo provider_xd lib_summary.mrs_token_deps;
                mpls_direct_providers = lib_summary.mrs_direct_providers;
              })
      in
      Hashtbl.replace cache provider.pi_file lib_summary;
      lib_summary

let menhir_provider_closure yo xd cache providers =
  let provider_tokens_rev = ref [] in
  let seen_providers = Hashtbl.create 16 in
  let providers_rev = ref [] in
  let rec visit_provider provider =
    let first_visit =
      match Hashtbl.find_opt seen_providers provider.pi_file with
      | None ->
          Hashtbl.replace seen_providers provider.pi_file provider;
          providers_rev := provider :: !providers_rev;
          true
      | Some existing ->
          if Import_backend.module_id (Menhir yo) existing
             <> Import_backend.module_id (Menhir yo) provider then
            Auxl.error None
              ("menhir output: inconsistent OCaml paths for imported provider "
               ^ provider.pi_file ^ "\n");
          false
    in
    if first_visit then begin
      let lib_summary =
        menhir_provider_lib_summary yo xd cache provider
      in
      provider_tokens_rev :=
        List.rev_append lib_summary.mpls_tokens !provider_tokens_rev;
      List.iter visit_provider lib_summary.mpls_direct_providers
    end
  in
  List.iter visit_provider providers;
  (List.rev !providers_rev,
   dedup_token_entries (List.rev !provider_tokens_rev))

(** ******************************************************************** *)
(** ocamllex                                                             *)
(** ******************************************************************** *)

(* output an ocamllex lexing rule for a token *)
let lex_token_argument_variable_of_mvr  mvr = mvr 

let pp_lex_token fd entry =
  match entry.te_kind with
  | TK_terminal -> 
      (match entry.te_subject with
       | TS_terminal t ->
           Printf.fprintf fd "| \"%s\"\n    { %s }\n"
             (String.escaped t) entry.te_name
       | _ -> raise (Failure "pp_lex_token"))
  | TK_metavar(ocaml_type, Some ocamllex_hom, ocamllex_of_string_hom_opt) ->
      (match entry.te_subject with
       | TS_metavar (_, t) ->
           let token_var =
             match entry.te_var_name with
             | Some name -> name
             | None -> t
           in
           let tv = lex_token_argument_variable_of_mvr token_var in
           Printf.fprintf fd "| %s as %s\n    { %s (%s) }\n"
             ocamllex_hom tv entry.te_name
             ((match ocamllex_of_string_hom_opt with
               | None ->
                   (match ocaml_type with
                    | "string" -> ""
                    | "int" -> "int_of_string"
                    | "float" -> "float_of_string"
                    | "bool" -> "bool_of_string")
               | Some f -> f)
              ^ " " ^ tv)
       | _ -> raise (Failure "pp_lex_token"))
(*         | _ -> tv)*)
  | TK_metavar(ocaml_type, None, _) ->
      Printf.fprintf fd "(* lexer rule for %s suppressed by ocamllex-remove *)\n"
        entry.te_name

(* output an ocamllex source file *)
let pp_lex_systemdefn m sd oi =
  let yo = match m with Lex yo -> yo | _ -> raise (Failure "pp_menhir_systemdefn called with bad ppmode") in 
  match oi with
  | (o,is)::[] ->
      let _ = Auxl.filename_check m o in
      let fd = open_out o in
      let xd = sd.syntax in
      let local_view = local_backend_view xd sd.structure in
(*      let tl = get_terminals sd.syntax in*)
      let comment_start =
        comment_start_of_local_rules yo local_view.lbv_rules
      in
      let ts_to_emit =
        token_entries_of_deps yo xd
          (token_deps_of_rules yo
             (close_rule_set local_view.lbv_rules xd.xd_rs
                (menhir_rule_dependencies yo xd)))
      in
      Printf.fprintf fd "(* generated by Ott %s from: %s *)\n" Version.n sd.sources;
      output_string fd ("{\n" ^ "open " ^ yo.ppm_caml_parser_module ^ "\n" ^ "exception Error of string\n" ^ "}\n\n");
      output_string fd "rule token = parse\n";
      output_string fd 
"| [' ' '\\t']
    { token lexbuf }
";
      output_string fd
"| '\n'
   { Lexing.new_line lexbuf; token lexbuf }
";
      output_string fd 
("| \"" ^ comment_start ^ "\" [^'\\n']* '\\n'
    { Lexing.new_line lexbuf; token lexbuf }
");
      output_string fd 
"| eof
    { EOF }
";
      List.iter (pp_lex_token fd) ts_to_emit;
      output_string fd 
"| _
    { raise (Error (Printf.sprintf \"At offset %d: unexpected character.\\n\" (Lexing.lexeme_start lexbuf))) }
"
;
      output_string fd "\n\n{\n}\n\n";
      close_out fd
  | _ -> Auxl.error None "must specify only one output file in the lex backend.\n"


(** ******************************************************************** *)
(** menhir                                                               *)
(** ******************************************************************** *)


(* output a menhir token definition *)
let pp_menhir_token fd entry =
  match entry.te_kind with
  | TK_terminal -> 
      (match entry.te_subject with
       | TS_terminal t ->
           Printf.fprintf fd "%%token %s  (* %s *)\n"
             entry.te_name
             (if t <> String.escaped t then entry.te_name else t)
       | _ -> raise (Failure "pp_menhir_token"))
  | TK_metavar(ocaml_type, ocamllex_hom_opt, ocamllex_of_string_opt) ->
      Printf.fprintf fd "%%token <%s> %s\n" ocaml_type entry.te_name

(* construct the ids used in menhir semantic actions to refer to values of production elements *)
let menhir_nonterminal_id_of_ntr ntr = ntr
(* Menhir docs say the following, which we do not currently ensure:
(It is recommended that the name of a nonterminal symbol begin with a
lowercase letter, so it falls in the category lid. This is in fact
mandatory for the start symbols.) *)

let menhir_sanitize_id s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'A' .. 'Z' -> Buffer.add_char b (Char.lowercase_ascii c)
    | 'a' .. 'z'
    | '0' .. '9'
    | '_' -> Buffer.add_char b c
    | _ -> Buffer.add_char b '_'
  ) s;
  Buffer.contents b

let menhir_namespace_of_source_file source_file =
  Digest.to_hex (Digest.string (canonical_source_file source_file))

let menhir_export_name_of_source_file source_file root =
  "ott_"
  ^ menhir_namespace_of_source_file source_file
  ^ "_"
  ^ menhir_sanitize_id root

let menhir_ast_alias_name_of_source_file source_file =
  "Ott_ast_" ^ menhir_namespace_of_source_file source_file

(* menhir nonterminal for synthesised start rules, which have an EOF added *)
let menhir_start ntr = ntr ^ "_start"

let menhir_semantic_value_id_of_ntmv ((ntmvr,suffix) as ntmv) = 
  let escape s =
    String.concat "" 
      (List.map 
         (fun c -> match c with
         | '\'' ->"_prime"             (* ignoring possibility of collision*)
         | _ -> String.make 1 c) 
         (Auxl.char_list_of_string s)) in
  let string_of_suffix_item si = match si with
  | Si_num s -> s
  | Si_punct s -> s
  | Si_var (sv,i) -> "Si_var_UNIMPLEMENTED"
  | Si_index i -> string_of_int i in (* ignoring possible name clashes...*)
  let result = ntmvr ^ String.concat "" (List.map string_of_suffix_item suffix) in
  escape (String.uncapitalize_ascii result)

let menhir_semantic_value_id_of_list es = 
  let f e = match e with
  | Lang_terminal t -> None
  | Lang_nonterm (ntr,nt) ->
      Some (menhir_semantic_value_id_of_ntmv nt)
  | Lang_metavar (mvr,mv) ->
      Some (menhir_semantic_value_id_of_ntmv mv)
  | Lang_list elb -> 
      raise (Failure "unexpected nested Lang_list")
  | _ ->
      raise (Failure "unexpected other Lang_ form") in
  let ss = Auxl.option_map f es in
  match ss with
  | [] -> raise (Failure "List form consisting only of terminals not supported")
  | _ -> String.concat "_" ss


type pp_kind =
  | Ppk_raw
  | Ppk_pretty
  | Ppk_json

(* ocaml pp function names *)

let pp_kind_fun_name kind ntmvr =
  match kind with
  | Ppk_raw -> "pp_raw_" ^ ntmvr
  | Ppk_pretty -> "pp_" ^ ntmvr
  | Ppk_json -> "pp_json_" ^ ntmvr

let pp_pp_raw_name ntmvr =
  pp_kind_fun_name Ppk_raw ntmvr

let pp_pp_json_name ntmvr =
  pp_kind_fun_name Ppk_json ntmvr

let pp_pp_name ntmvr =
  pp_kind_fun_name Ppk_pretty ntmvr

let pp_kind_hom_name kind =
  match kind with
  | Ppk_raw -> "pp-raw"
  | Ppk_pretty -> "pp"
  | Ppk_json -> "pp-json"

let enabled_pp_kinds () =
  if !Global_option.caml_pp_json then
    [Ppk_raw; Ppk_pretty; Ppk_json]
  else
    [Ppk_raw; Ppk_pretty]

let let_rec_block defs =
  match defs with
  | [] -> ""
  | _ -> "let rec " ^ String.concat "and " defs

let string_of_hom_spec_el hse =
  match hse with
  | Hom_string s -> s
  | Hom_index i -> raise (Failure "string_of_hom_spec")
  | Hom_terminal t -> t
  | Hom_ln_free_index _ -> raise (Failure "string_of_hom_spec")

let string_of_hom_spec hs =
  String.concat "" (List.map string_of_hom_spec_el hs)

type ocaml_hom_kind =
  | OH_string
  | OH_int
  | OH_float
  | OH_bool
  | OH_big_int
  | OH_custom of string
  | OH_complex of hom_spec_el list
  | OH_missing

let classify_ocaml_hom reps =
  match Auxl.hom_spec_for_hom_name "ocaml" reps with
  | Some [Hom_string "string"] -> OH_string
  | Some [Hom_string "int"] -> OH_int
  | Some [Hom_string "float"] -> OH_float
  | Some [Hom_string "bool"] -> OH_bool
  | Some [Hom_string "big_int"] -> OH_big_int
  | Some [Hom_string s] -> OH_custom s
  | Some hs -> OH_complex hs
  | None -> OH_missing

                                              
let pp_params r =
  match Auxl.hom_spec_for_hom_name "pp-params" r.rule_homs with 
  | Some hs -> 
      " " ^ string_of_hom_spec hs
  | None ->
      ""
      

            
(* construct all the data we need, for parsing and pretty printing,
from an element of an ott production *)

type element_data = {
    semantic_value_id : string option; (* None for terminals, Some ntmv for nonterms and metavars, Some compound_id for lists *)
    grammar_body : string;
    semantic_action : string option;  (* None for terminals *)
    pp_raw_rhs : string option;       (* None for terminals *)
    pp_pretty_rhs : string option;
    pp_json_rhs : string option;  (* None for terminals *)
  }

let pp_json_key s = "\\\"" ^ s ^ "\\\""

let unique_semantic_value_ids semantic_value_ids =
  let unique_names = Auxl.ensure_unique_names (Auxl.option_map (fun id -> id) semantic_value_ids) in
  let rec assign semantic_value_ids unique_names =
    match semantic_value_ids with
    | [] -> []
    | None :: semantic_value_ids' ->
        None :: assign semantic_value_ids' unique_names
    | Some _ :: semantic_value_ids' ->
        (match unique_names with
        | unique_name :: unique_names' ->
            Some unique_name :: assign semantic_value_ids' unique_names'
        | [] ->
            raise (Failure "unique_semantic_value_ids")) in
  assign semantic_value_ids unique_names

let semantic_value_id_of_element e =
  match e with
  | Lang_terminal _ -> None
  | Lang_nonterm (_,nt) -> Some (menhir_semantic_value_id_of_ntmv nt)
  | Lang_metavar (_,mv) -> Some (menhir_semantic_value_id_of_ntmv mv)
  | Lang_list elb -> Some (menhir_semantic_value_id_of_list elb.elb_es)
  | _ -> raise (Failure "unexpected Lang_ form")

let required_semantic_value_id kind = function
  | Some id -> id
  | None -> raise (Failure ("missing semantic value id for " ^ kind))

let pp_pretty_rhs_if_not_suppressed homs rhs =
  match Auxl.hom_spec_for_hom_name "pp-suppress" homs with
  | Some _ -> None
  | None -> Some rhs

type pp_target =
  | Local_pp of nontermroot
  | Imported_pp of imported_printer_dep

type resolved_nonterm = {
  rn_actual_ntr : nontermroot;
  rn_runtime_check : string option;
  rn_rule : rule;
  rn_pp_target : pp_target;
}

let imported_pp_target actual_rule imported_dep =
  Imported_pp (validated_imported_pp_dep actual_rule imported_dep)

let resolved_nonterm xd ~source_file ntr =
  let resolved_ref = resolved_nonterm_ref xd ~source_file ntr in
  let pp_target =
    match resolved_ref.rnr_imported_dep with
    | None -> Local_pp resolved_ref.rnr_actual_ntr
    | Some imported_dep ->
        imported_pp_target resolved_ref.rnr_rule imported_dep
  in
  {
    rn_actual_ntr = resolved_ref.rnr_actual_ntr;
    rn_runtime_check = resolved_ref.rnr_runtime_check;
    rn_rule = resolved_ref.rnr_rule;
    rn_pp_target = pp_target;
  }

let validate_rule_menhir_visibility r =
  if has_hom "menhir-public" r.rule_homs && has_hom "menhir-suppress" r.rule_homs then
    Auxl.error (Some r.rule_loc)
      ("menhir output: rule " ^ r.rule_ntr_name
       ^ " cannot have both {{ menhir-public }} and {{ menhir-suppress }}\n")

let validate_menhir_summary xd summary =
  List.iter validate_rule_menhir_visibility xd.xd_rs;
  List.iter (fun imported_use ->
    if not (has_hom "menhir-public" imported_use.miu_actual_rule.rule_homs) then
      Auxl.error (Some imported_use.miu_use_loc)
        ("menhir output: imported nonterminal " ^ imported_use.miu_requested_ntr
         ^ " requires provider rule " ^ imported_use.miu_actual_ntr
         ^ " to have {{ menhir-public }}\n")
  ) summary.mrs_imported_uses

type imported_pp_provider_names = {
  ipp_public_path : string;
  ipp_binder_name : string;
}

type imported_pp_naming = (provider_info, imported_pp_provider_names) Hashtbl.t

let imported_pp_public_path yo provider =
  Import_backend.module_id (Menhir yo) provider

let imported_pp_binder_stem_of_public_path public_path =
  let b = Buffer.create (String.length public_path) in
  String.iter (fun c ->
    match c with
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '_' -> Buffer.add_char b c
    | _ -> Buffer.add_char b '_'
  ) public_path;
  let stem = Buffer.contents b in
  let stem =
    if stem = "" then
      "Imported"
    else
      match stem.[0] with
      | 'A' .. 'Z' -> stem
      | 'a' .. 'z' -> String.capitalize_ascii stem
      | _ -> "Imported_" ^ stem
  in
  stem ^ "_pp"

let imported_pp_providers deps =
  let seen = Hashtbl.create 8 in
  let providers_rev = ref [] in
  List.iter (fun dep ->
    let provider = dep.ip_provider in
    if not (Hashtbl.mem seen provider) then begin
      Hashtbl.replace seen provider ();
      providers_rev := provider :: !providers_rev
    end
  ) deps;
  List.rev !providers_rev

let imported_pp_naming yo deps =
  let providers = imported_pp_providers deps in
  let public_paths = List.map (imported_pp_public_path yo) providers in
  let binder_names =
    Auxl.ensure_unique_names
      (List.map imported_pp_binder_stem_of_public_path public_paths)
  in
  let naming = Hashtbl.create (List.length providers) in
  List.iter2 (fun provider (public_path, binder_name) ->
    Hashtbl.replace naming provider
      { ipp_public_path = public_path; ipp_binder_name = binder_name }
  ) providers (List.combine public_paths binder_names);
  naming

let imported_pp_provider_names naming provider =
  match Hashtbl.find_opt naming provider with
  | Some names -> names
  | None ->
      Auxl.int_error
        ("missing imported Menhir PP naming for provider " ^ provider.pi_module)

let imported_pp_call naming dep pp_name pp_arg =
  "Imported."
  ^ (imported_pp_provider_names naming dep.ip_provider).ipp_binder_name
  ^ "."
  ^ pp_name dep.ip_origin_name
  ^ pp_arg

let pp_target_call naming target pp_name pp_arg =
  match target with
  | Local_pp ntr -> pp_name ntr ^ pp_arg
  | Imported_pp dep -> imported_pp_call naming dep pp_name pp_arg

let menhir_public_rule_name r =
  menhir_export_name_of_source_file
    (Import_naming.source_file_of_loc ~default:"" r.rule_loc)
    r.rule_ntr_name

let menhir_private_rule_name r =
  "ott_private_"
  ^ menhir_namespace_of_source_file
      (Import_naming.source_file_of_loc ~default:"" r.rule_loc)
  ^ "_"
  ^ menhir_sanitize_id r.rule_ntr_name

type menhir_rule_naming =
  | Menhir_root_rules
  | Menhir_lib_rules

let menhir_is_public_rule r =
  has_hom "menhir-public" r.rule_homs

let menhir_local_rule_name naming r =
  match naming with
  | Menhir_root_rules ->
      menhir_nonterminal_id_of_ntr r.rule_ntr_name
  | Menhir_lib_rules ->
      if menhir_is_public_rule r then
        menhir_public_rule_name r
      else
        menhir_private_rule_name r

let menhir_rule_header_name naming r =
  let rule_name = menhir_local_rule_name naming r in
  match naming with
  | Menhir_root_rules -> rule_name
  | Menhir_lib_rules ->
      if menhir_is_public_rule r then
        "%public " ^ rule_name
      else
        rule_name

let menhir_nonterminal_of_target naming target actual_rule =
  match target with
  | Local_pp _ -> menhir_local_rule_name naming actual_rule
  | Imported_pp dep ->
      menhir_export_name_of_source_file dep.ip_provider.pi_file dep.ip_origin_name

let rec element_data_of_elements yo xd ts imported_pp_naming naming source_file
    (allow_lists:bool) (indent_nonterms:bool) (no_json_key : bool) es =
  let semantic_value_ids = unique_semantic_value_ids (List.map semantic_value_id_of_element es) in
  List.map2
    (element_data_of_element yo xd ts imported_pp_naming naming source_file
       allow_lists indent_nonterms no_json_key)
    semantic_value_ids es

and element_data_of_element yo xd ts imported_pp_naming naming source_file
    (allow_lists:bool) (indent_nonterms:bool) (no_json_key : bool)
    semantic_value_id e
  : element_data =
(*string option(*semantic_value_id*) * string(*grammar body*) * string option (*semantic action*) =*)
  match e with
  | Lang_terminal t -> 
      { semantic_value_id = None;
        grammar_body      = token_of_terminal ts t;
        semantic_action   = None;
        pp_raw_rhs        = None;
        pp_json_rhs       = None;
        pp_pretty_rhs     = Some ("string \"" ^ String.escaped t ^ "\""); }

  | Lang_nonterm (ntr,nt) ->
      let svi = required_semantic_value_id "Lang_nonterm" semantic_value_id in
      let resolved = resolved_nonterm xd ~source_file ntr in
      let actual_rule = resolved.rn_rule in
      let params = pp_params actual_rule in
      let pp_arg = params ^ " " ^ svi in
      let pp_call = pp_target_call imported_pp_naming resolved.rn_pp_target in
      let actual_semantic_action = 
        match resolved.rn_runtime_check with
        | Some check -> Some ("(if " ^ check ^ " " ^ svi ^ " then " ^ svi ^ " else raise Parsing.Parse_error)")
        | None -> Some svi in
      let json_prefix =
        if no_json_key then
          ""
        else
          "string \"" ^ pp_json_key (Grammar_pp.pp_plain_nonterm nt)  ^ ":\" ^^ "
      in
      let pp_json_rhs =
        Some (json_prefix ^ pp_call pp_pp_json_name pp_arg)
      in
      let pp_pretty_rhs =
        pp_pretty_rhs_if_not_suppressed actual_rule.rule_homs
          (let rhs = pp_call pp_pp_name pp_arg in
           if indent_nonterms then
             "nest 2 (" ^ rhs ^ ")"
           else
             rhs)
      in
      { semantic_value_id = Some svi;
        grammar_body      =
          menhir_nonterminal_of_target naming resolved.rn_pp_target
            actual_rule;
        semantic_action   = actual_semantic_action;
        pp_raw_rhs        = Some (pp_call pp_pp_raw_name pp_arg);
        pp_json_rhs       = pp_json_rhs;
        pp_pretty_rhs     = pp_pretty_rhs }

  | Lang_metavar (mvr,mv) -> (* assuming all metavars map onto string-containing tokens *)
      let svi = required_semantic_value_id "Lang_metavar" semantic_value_id in
      let mvd = Auxl.mvd_of_mvr_nonprimary xd mvr in
      { semantic_value_id = Some svi;
        grammar_body      = token_of_metavarroot ts ~source_file mvr;
        semantic_action   = Some svi;
        pp_raw_rhs        = Some (pp_pp_raw_name mvr ^ " " ^ svi);
        pp_json_rhs       = Some ( "string \"" ^ pp_json_key (Grammar_pp.pp_plain_metavar mv) ^ " : \" ^^ " ^ pp_pp_json_name mvr ^ " " ^ svi);
        pp_pretty_rhs     = pp_pretty_rhs_if_not_suppressed mvd.mvd_rep (pp_pp_name mvr ^ " " ^ svi); }
(*        pp_raw_rhs        = Some (" string \"\\\"\" ^^ string " ^ svi ^ " ^^ string \"\\\"\"");
        pp_pretty_rhs     = "string "^ svi ; }
 *)
      
  | Lang_list elb ->
      if not allow_lists then raise (Failure "unexpected list form");
      let element_data =
        element_data_of_elements yo xd ts imported_pp_naming naming
          source_file false true
          indent_nonterms elb.elb_es
      in
      
      let svi = required_semantic_value_id "Lang_list" semantic_value_id in
      let element_svis = Auxl.option_map (function x -> x.semantic_value_id) element_data in      

      let body = 
        let list_grammar_constructor body = 
          let terminal_option = 
            match elb.elb_tmo with
            | None -> None
            | Some t -> Some (token_of_terminal ts t) in
          let non_empty = 
            match elb.elb_boundo with
            | Some (Bound_dotform bd)   -> bd.bd_length 
            | Some (Bound_comp bc)      -> 0
            | Some (Bound_comp_u bcu)   -> 0
            | Some (Bound_comp_lu bclu) -> bclu.bclu_length 
            | None                      -> 0 in
          match (non_empty, terminal_option) with
          | (0,Some t) -> "separated_list(" ^ t ^ "," ^ body ^ ")"
          | (0,None)   -> "list(" ^ body ^ ")" 
          | (1,Some t) -> "separated_nonempty_list(" ^ t ^ "," ^ body ^ ")"
          | (1,None)   -> "nonempty_list(" ^ body ^ ")"
          | (2,Some t) -> "separated_nonempty2_list(" ^ t ^ "," ^ body ^ ")"
          | (2,None)   -> "nonempty2_list(" ^ body ^ ")"
          | (_,_)      -> Auxl.error None ("unexpected length in pp_menhir_element") 
        in
        let body0 = 
          (match element_data with
          | [] -> raise (Failure "unexpected empty list form")
          | [x] -> x.grammar_body;
          | _ -> 
              Printf.sprintf "tuple%d(%s)" (List.length element_data) (String.concat "," (List.map (function x->x.grammar_body) element_data))) in
        list_grammar_constructor body0 in
      
      let pat = "(" ^ String.concat "," element_svis ^")" in
      let action = 
        (* if need be (but not otherwise) project out unit elements from terminals within list *)
        if List.exists (function x-> x.semantic_value_id = None)  element_data  then 
          let pat =
            String.concat ","
              (List.map
                 (function x -> match x.semantic_value_id with Some svi->svi | None->"()")
                 element_data) in
          let rhs = String.concat "," element_svis in
          "List.map (function ("^pat^") -> ("^rhs^")) "^svi
        else 
          svi  in

      let list_map rhs = "List.map (function "^pat^" -> "^rhs^") " ^ svi in
      
      let pp_raw_rhs = 
        let rhs_data = Auxl.option_map (function x-> x.pp_raw_rhs) element_data in
        let rhs =  "string \"(\" ^^ " ^ String.concat  " ^^ string \",\" ^^ " rhs_data ^ " ^^ string \")\"" in
        let pper = "string \"[\" ^^ separate  (string \";\") (" ^ list_map rhs ^ ")" ^" ^^ string \"]\"" in
        pper in

      let pp_json_rhs = 
        let rhs_data = Auxl.option_map (function x-> x.pp_json_rhs) element_data in
        let rhs =  "string \"\" ^^ " ^ String.concat  " ^^ string \",\" ^^ " rhs_data ^ " ^^ string \"\"" in
        let pper = "string \"\\\"list\\\" : [\" ^^ separate  (string \",\") (" ^ list_map rhs ^ ")" ^" ^^ string \"]\"" in
        pper in

      
      let pp_pretty_rhs = 
        let rhs_data = Auxl.option_map (function x-> x.pp_pretty_rhs) element_data in
        let rhs =  String.concat  " ^^ string \" \" ^^ " rhs_data in
        let sep = match elb.elb_tmo with Some t -> "(string \""^String.escaped t^"\")" | None -> "(break 1)" in
        let pper = "group(separate " ^  sep ^ " (" ^ list_map rhs ^ "))" in
        pper in

      { semantic_value_id = Some svi;
        grammar_body      = body;
        semantic_action   = Some action;
        pp_raw_rhs        = Some pp_raw_rhs;
        pp_json_rhs       = Some pp_json_rhs;
        pp_pretty_rhs     = Some pp_pretty_rhs ; }
        
  | _ -> raise (Failure "unexpected Lang_ form")

let element_data_of_prod yo xd ts imported_pp_naming naming r p =
  (* try indenting nonterms iff this production has a top-level terminal *)
  let indent_nonterms = List.exists (function | Lang_terminal _ -> true | _ -> false) p.prod_es in 
  element_data_of_elements yo xd ts imported_pp_naming naming
    (menhir_rule_source r)
    true indent_nonterms
    false p.prod_es


let pp_menhir_prod_grammar element_data = 
  String.concat "  " 
    (List.map 
       (function x -> 
         match x.semantic_value_id with 
         | Some svi -> Printf.sprintf "%s = %s" svi (x.grammar_body) 
         | None -> x.grammar_body) 
       element_data)

let pp_menhir_prod_action p element_data = 
  String.capitalize_ascii p.prod_name 
  ^ 
    (let args = Auxl.option_map (function x-> x.semantic_action) element_data in
    match args with
    | [] -> ""
    | _ -> "("^ String.concat "," args ^ ")" )

(* aux rule stuff *)
(* ...for rules *)
let generate_aux_info_for_rule generate_aux_info r = 
  generate_aux_info &&
  (match Auxl.hom_spec_for_hom_name "aux" r.rule_homs with 
  | Some hs -> true
  | None -> false)

let aux_constructor generate_aux_info_here r p : string option = 
  if generate_aux_info_here && not(has_hom "ocaml" p.prod_homs) && !Global_option.aux_style_rules then
    let aux_prod_name = (if r.rule_pn_wrapper<>"" then r.rule_pn_wrapper else String.capitalize_ascii r.rule_ntr_name ^"_") ^ "aux" in
    Some aux_prod_name
  else
    None
(* ...for constructors *)

let ott_menhir_loc = "ott_menhir_loc" (* ocaml variable to use for locations *)

let aux_constructor_element : element_data = 
  { semantic_value_id = Some ott_menhir_loc;
    grammar_body = "DUMMY";
    semantic_action = Some "Range($symbolstartpos,$endpos)";
    pp_raw_rhs = None;
    pp_json_rhs = None;
    pp_pretty_rhs = None (* effectively pp-suppress for this element *); }

let generate_aux_info_for_prod generate_aux_info r p = 
  generate_aux_info && not(!Global_option.aux_style_rules) && 
  (match Auxl.hom_spec_for_hom_name "aux" r.rule_homs with 
  | Some hs -> true
  | None -> false)


let pp_pattern_prod r p generate_aux_info_here element_data = 
  let element_data' = element_data @ if generate_aux_info_for_prod generate_aux_info_here r p then [aux_constructor_element] else [] in
  let inner_pattern = 
    String.capitalize_ascii p.prod_name 
    ^ 
      (let args = Auxl.option_map (function x-> x.semantic_value_id) element_data' in
      match args with
      | [] -> ""
      | _ -> "("^ String.concat "," args ^ ")" )
  in
  match aux_constructor generate_aux_info_here r p with
  | Some aux_con -> aux_con ^ "(" ^ inner_pattern ^ "," ^ ott_menhir_loc^")"
  | None -> inner_pattern



let menhir_prec_spec homs = 
  match Auxl.hom_spec_for_hom_name "menhir-prec" homs with 
  | None -> ""
  | Some hs -> 
      let pp_hse hse = 
        match hse with
        | Hom_string s ->  s
        | Hom_index i -> raise (Failure ("pp_menhir_prec_spec Hom_index"))
        | Hom_terminal s -> s
        | Hom_ln_free_index (mvs,s) -> raise (Failure "pp_menhir_prec_spec Hom_ln_free_index")  in
      "%prec " ^ String.concat "" (List.map pp_hse hs) ^ "\n"

let menhir_wrap_action r action =
  "let open "
  ^ menhir_ast_alias_name_of_source_file (menhir_rule_source r)
  ^ " in "
  ^ action





(* build a menhir production *)
let pp_menhir_prod yo imported_pp_naming wrap_action_in_provider_alias
    naming
    generate_aux_info_here xd ts r p =
  if suppress_prod yo p then 
    ""
  else

    (* pp the production source, to use in comment *)
    let m_ascii = Ascii { ppa_colour = false; 
		          ppa_lift_cons_prefixes = false; 
		          ppa_ugly= false; 
		          ppa_show_deps = false; 
		          ppa_show_defns = false } in
    let ppd_prod =
      let stnb = Grammar_pp.canonical_symterm_node_body_of_prod r.rule_ntr_name p in
      let st = St_node(dummy_loc,stnb) in
      Grammar_pp.pp_symterm m_ascii xd [] de_empty st in
    let ppd_comment = "(* "^ppd_prod ^ " :: " ^ p.prod_name^" *)" in

    (* now the real work *)
    let element_data =
      element_data_of_prod yo xd ts imported_pp_naming naming r p
    in 
    let element_data' = element_data @ if generate_aux_info_for_prod generate_aux_info_here r p then [aux_constructor_element] else [] in
    let ppd_action = 
      if p.prod_sugar || (has_hom "quotient-remove" p.prod_homs && has_hom "ocaml" p.prod_homs) || r.rule_phantom || has_hom "ocaml" r.rule_homs then 
        (* ocaml hom case *)
        (* to do the proper escaping of nonterms within the hom, we need to pp here, not reuse the standard machinery *)
"(*Case 1*) " ^ 
        let hs = (match Auxl.hom_spec_for_hom_name "ocaml" p.prod_homs with Some hs -> hs | None -> raise (Failure ("no ocaml hom for "^p.prod_name))) in
        let element_data_hom_order =
          let filtered_element_data =
            List.filter
              (function
                | { semantic_value_id = Some _; _ } -> true
                | { semantic_value_id = None; _ } -> false)
              element_data in
          try
            let oh = List.assoc "order" p.prod_homs in
            let ohi = Auxl.option_map (fun hse -> match hse with Hom_index i -> Some i | _ -> None) oh in
            List.map (fun i -> List.nth filtered_element_data i) ohi
          with Not_found ->
            filtered_element_data in
        let pp_menhir_hse hse = 
          match hse with
          | Hom_string s -> s
          (* TODO, arbitrary failure? *)
          | Hom_index i ->
              let d = List.nth element_data_hom_order i in
              (match d.semantic_action with Some s -> s | None -> raise (Failure ("pp_menhir_hse Hom_index " ^ string_of_int i ^ " at " ^ Location.pp_loc p.prod_loc)))
          | Hom_terminal s -> s
          | Hom_ln_free_index (mvs,s) -> raise (Failure "Hom_ln_free_index not implemented")  in
        String.concat "" (List.map pp_menhir_hse hs)

       (*  let m' = Caml { Types.ppo_include_terminals=false; Types.caml_library = ref ("",[]) } in *)
       (*  let pp_prod m'= *)
       (*    let stnb = Grammar_pp.canonical_symterm_node_body_of_prod r.rule_ntr_name p in *)
       (*    let st = St_node(dummy_loc,stnb) in *)
       (*    Grammar_pp.pp_symterm m' xd [] de_empty st  *)
       (*  in  *)
       (* (\* (match Grammar_pp.pp_elements m xd [] (Grammar_pp.apply_hom_order m xd p) (\*p.prod_es*\) false false true false with Some s -> s | None -> "None")*\) *)
       (*  pp_prod m' *)
      else if not(r.rule_phantom) then 
"(*Case 2*) " ^         pp_menhir_prod_action p element_data'
      else (* use the ocaml hom - is this code now defunct? *)
"(*Case 3*) " ^         
        (match Auxl.hom_spec_for_hom_name "ocaml" p.prod_homs with 
        | Some hom -> 

            let m_hol = Hol { Types.hol_library = ref ("",[]) } in
            let m_ocaml = Caml { Types.ppo_include_terminals=false; Types.caml_library = ref ("",[]) } in
            let m_ascii = Types.error_opts in 
           Grammar_pp.pp_hom_spec m_hol (*m_ocaml*) (*(Menhir yo)*) xd hom
       (*  let pp_prod m'= *)
       (*    let stnb = Grammar_pp.canonical_symterm_node_body_of_prod r.rule_ntr_name p in *)
       (*    let st = St_node(dummy_loc,stnb) in *)
       (*    Grammar_pp.pp_symterm m' xd [] de_empty st *)
       (*  in *)
       (* (\* (match Grammar_pp.pp_elements m xd [] (Grammar_pp.apply_hom_order m xd p) (\*p.prod_es*\) false false true false with Some s -> s | None -> "None")*\) *)
       (*  pp_prod (\*m_ocaml*\) m_ascii *)



        | None -> ignore(Auxl.error (Some (r.rule_loc)) ("no ocaml hom for production "^p.prod_name));"")
    in

    let aux_wrapper_l, aux_wrapper_r = 
      (match aux_constructor generate_aux_info_here r p with 
      | Some aux_con -> 
          (aux_con ^ "(",
       ",Range($symbolstartpos,$endpos) )")
      | None -> 
          ("", "")) in
    let semantic_action_body =
      aux_wrapper_l ^ ppd_action ^ aux_wrapper_r
    in
    let semantic_action =
      if wrap_action_in_provider_alias then
        menhir_wrap_action r semantic_action_body
      else
        semantic_action_body
    in

    "| " ^ pp_menhir_prod_grammar element_data ^ "    " ^ ppd_comment ^ "\n"
    ^ 
      "    { " ^ semantic_action ^ " }\n"
    ^ 
      menhir_prec_spec p.prod_homs



(* build a menhir rule *)
let pp_menhir_rule yo imported_pp_naming wrap_action_in_provider_alias
    naming generate_aux_info xd ts r =
  with_loc_source r.rule_loc (fun () ->
    if suppress_rule yo r then 
      ""
    else 
      (* ignore the body of the aux hom - assume it is {{ aux _ l }} *)
      let generate_aux_info_here = generate_aux_info_for_rule generate_aux_info r in 
       menhir_rule_header_name naming r ^ ":\n" 
      ^  String.concat ""
           (List.map
             (pp_menhir_prod yo imported_pp_naming wrap_action_in_provider_alias
                naming generate_aux_info_here xd ts r)
              r.rule_ps)
      ^ "\n")

let is_start_rule yo r = 
  not(suppress_rule yo r) && (has_hom "menhir-start" r.rule_homs) && not (has_hom "quotient-with" r.rule_homs)

(* construct a menhir rule for a start symbol, sticking an EOF on *)
let pp_menhir_start_rule yo xd ts r = 
  with_loc_source r.rule_loc (fun () ->
    if not(is_start_rule yo r) then 
      ""
    else 
      let ntr = r.rule_ntr_name in
      let nt = (ntr,[]) in
      let id,body =
        menhir_semantic_value_id_of_ntmv nt,
        menhir_nonterminal_id_of_ntr ntr
      in 
      menhir_start id ^ ":\n" 
      ^ "| " ^ Printf.sprintf "%s = %s" id body ^ " EOF" ^ "\n"
      ^ "    { " ^ id ^ " }\n\n")

let pp_menhir_start_rules yo xd ts rs = 
  String.concat "" 
    (List.map (pp_menhir_start_rule yo xd ts) rs)
                                                                
(* construct menhir start symbol declarations *)
  let pp_menhir_start_symbols yo generate_aux_info xd rs = 
    String.concat "" 
      (List.map 
         (function r -> 
           with_loc_source r.rule_loc (fun () ->
             if not(is_start_rule yo r) then 
               ""
             else
               let ty0 = 
                 Grammar_pp.strip_surrounding_parens 
                   (Grammar_pp.pp_nontermroot_ty (Caml yo.ppm_caml_opts) xd r.rule_ntr_name) in
               let ty =
                 match Auxl.hom_spec_for_hom_name "menhir-start-type" r.rule_homs with 
                 | Some hs ->
                     Grammar_pp.pp_hom_spec_in_type_context (Menhir yo) xd hs
                 | None ->
                     let (ty1, ty_arg) = 
                       if String.length ty0 >= 3 && String.sub ty0 0 3 = "'a " then 
                         (String.sub ty0 3 (String.length ty0 - 3), "unit ")
                       else
                         (ty0, "") in
                     ty_arg 
                     ^ yo.ppm_caml_ast_module 
                     ^ "."  
                     ^ ty1
               in
               (*let generate_aux_info_here = generate_aux_info &&
                 (match Auxl.hom_spec_for_hom_name "aux" r.rule_homs with 
                 | Some hs -> true
                 | None -> false) in
                 let ty1 = if generate_aux_info_here then ty1 ^ "_aux" else ty1 in*)
               "%start <" 
               ^ ty
               ^ "> " 
               ^ menhir_start (menhir_nonterminal_id_of_ntr r.rule_ntr_name) 
               ^ "\n"))
         rs)

(** ******************************************************************** *)
(** raw pp                                                               *)
(** ******************************************************************** *)

      
(* all this should really use a more efficient representation than string *)

let pp_kind_element_rhs kind data =
  match kind with
  | Ppk_raw -> data.pp_raw_rhs
  | Ppk_pretty -> data.pp_pretty_rhs
  | Ppk_json -> data.pp_json_rhs

let pp_kind_suppress_metavar kind yo md =
  match kind with
  | Ppk_pretty -> false
  | Ppk_raw | Ppk_json -> suppress_metavar yo md

let pp_kind_suppress_rule kind yo r =
  suppress_rule yo r
  || (match kind with
      | Ppk_pretty -> has_hom "pp-suppress" r.rule_homs
      | Ppk_raw | Ppk_json -> false)

let pp_kind_default_metavar_rhs kind svi ocaml_kind =
  match kind, ocaml_kind with
  | Ppk_raw, OH_string
  | Ppk_json, OH_string ->
      " string \"\\\"\" ^^ string " ^ svi ^ " ^^ string \"\\\"\""
  | Ppk_pretty, OH_string ->
      "string " ^ svi ^ " ^^ string \"\""
  | Ppk_raw, OH_int
  | Ppk_pretty, OH_int ->
      "string (string_of_int " ^ svi ^ ")"
  | Ppk_json, OH_int ->
      "string_of_int " ^ svi
  | Ppk_raw, OH_big_int
  | Ppk_pretty, OH_big_int ->
      "string (Big_int.string_of_big_int " ^ svi ^ ")"
  | Ppk_json, OH_big_int ->
      "Big_int.string_of_big_int " ^ svi
  | (Ppk_raw | Ppk_pretty), (OH_float | OH_bool | OH_custom _) ->
      let fn =
        match ocaml_kind with
        | OH_float -> "string_of_float"
        | OH_bool -> "string_of_bool"
        | OH_custom s -> "string_of_" ^ s
        | _ -> raise (Failure "pp_kind_default_metavar_rhs")
      in
      "string (" ^ fn ^ " " ^ svi ^ ")"
  | Ppk_json, (OH_float | OH_bool | OH_custom _) ->
      let fn =
        match ocaml_kind with
        | OH_float -> "string_of_float"
        | OH_bool -> "string_of_bool"
        | OH_custom s -> "string_of_" ^ s
        | _ -> raise (Failure "pp_kind_default_metavar_rhs")
      in
      fn ^ " " ^ svi
  | _, (OH_complex _ | OH_missing) ->
      raise (Failure "pp_kind_default_metavar_rhs")

let pp_kind_missing_metavar_defn kind md =
  match classify_ocaml_hom md.mvd_rep with
  | OH_complex hs ->
      Some
        ((match kind with
          | Ppk_pretty -> "no pp default for "
          | Ppk_raw | Ppk_json -> "no default for ")
         ^ md.mvd_name
         ^ " ocaml homspec="
         ^ Grammar_pp.pp_plain_hom_spec hs
         ^ "\n\n")
  | OH_missing ->
      Some
        ((match kind with
          | Ppk_raw -> "no pp-raw or ocaml hom for "
          | Ppk_pretty -> "no pp or ocaml hom for "
          | Ppk_json -> "no pp-json or ocaml hom for ")
         ^ md.mvd_name
         ^ "\n\n")
  | ocaml_kind ->
      let svi = "x" in
      Some
        (pp_kind_fun_name kind md.mvd_name
         ^ " "
         ^ svi
         ^ " = "
         ^ pp_kind_default_metavar_rhs kind svi ocaml_kind
         ^ "\n\n")

let pp_kind_metavar_defn kind yo xd md =
  with_loc_source md.mvd_loc (fun () ->
    if pp_kind_suppress_metavar kind yo md then
      None
    else
      match Auxl.hom_spec_for_hom_name (pp_kind_hom_name kind) md.mvd_rep with
      | Some hs ->
          Some
            (pp_kind_fun_name kind md.mvd_name
             ^ " "
             ^ Grammar_pp.pp_hom_spec (Menhir yo) xd hs
             ^ "\n\n")
      | None ->
          pp_kind_missing_metavar_defn kind md)

let pp_kind_prod_rhs kind generate_aux_info_here r p element_data =
  let args = Auxl.option_map (pp_kind_element_rhs kind) element_data in
  match kind with
  | Ppk_raw ->
      (match aux_constructor generate_aux_info_here r p with
       | Some _ ->
           " string \"[\" ^^ string (pp_raw_l " ^ ott_menhir_loc ^ ") ^^ string \"]\" ^^ "
       | None -> "")
      ^ "string \"" ^ String.capitalize_ascii p.prod_name ^ "\""
      ^ (match args with
         | [] -> ""
         | _ ->
             " ^^ string \"(\" ^^ "
             ^ String.concat " ^^ string \",\" ^^ " args
             ^ " ^^ string \")\"")
  | Ppk_pretty ->
      (match args with
       | [] -> "string \"\""
       | [arg] -> arg
       | _ ->
           "group(string \"\" ^^ "
           ^ String.concat " ^^ break 1 ^^ " args
           ^ " ^^ string \"\")")
  | Ppk_json ->
      (match aux_constructor generate_aux_info_here r p with
       | Some _ ->
           " string \"[\" ^^ string (pp_json_l " ^ ott_menhir_loc ^ ") ^^ string \"]\" ^^ "
       | None -> "")
      ^ "string \"{ \\\"tag\\\" : \\\"" ^ String.capitalize_ascii p.prod_name ^ "\\\"\""
      ^ (match args with
         | [] -> " ^^ string \"}\""
         | _ ->
             " ^^ string \", \" ^^ "
             ^ String.concat " ^^ string \",\" ^^ " args
             ^ " ^^ string \"}\"")

let pp_kind_prod kind yo imported_pp_naming generate_aux_info_here xd ts r p =
  if suppress_prod yo p || p.prod_sugar then
    ""
  else
    match Auxl.hom_spec_for_hom_name (pp_kind_hom_name kind) p.prod_homs with
    | Some hs ->
        "| "
        ^ String.capitalize_ascii p.prod_name
        ^ " "
        ^ Grammar_pp.pp_hom_spec (Menhir yo) xd hs
        ^ "\n"
    | None ->
        let element_data =
          element_data_of_prod yo xd ts imported_pp_naming
            Menhir_root_rules r p
        in
        "| "
        ^ pp_pattern_prod r p generate_aux_info_here element_data
        ^ " -> "
        ^ pp_kind_prod_rhs kind generate_aux_info_here r p element_data
        ^ "\n"

let pp_kind_phantom_rule kind r =
  match kind with
  | Ppk_raw ->
      Some (pp_kind_fun_name kind r.rule_ntr_name ^ "_default \n\n")
  | Ppk_pretty ->
      let () =
        Auxl.warning (Some r.rule_loc)
          ("no pp hom for phantom production " ^ r.rule_ntr_name)
      in
      Some (pp_kind_fun_name kind r.rule_ntr_name ^ "_default \n\n")
  | Ppk_json ->
      Auxl.error (Some r.rule_loc)
        ("no pp-json hom for phantom production " ^ r.rule_ntr_name)

let pp_kind_rule kind yo imported_pp_naming generate_aux_info xd ts r =
  with_loc_source r.rule_loc (fun () ->
    if pp_kind_suppress_rule kind yo r then
      None
    else
      match Auxl.hom_spec_for_hom_name (pp_kind_hom_name kind) r.rule_homs with
      | Some hs ->
          Some
            (pp_kind_fun_name kind r.rule_ntr_name
             ^ " "
             ^ Grammar_pp.pp_hom_spec (Menhir yo) xd hs
             ^ "\n\n")
      | None ->
          if r.rule_phantom then
            pp_kind_phantom_rule kind r
          else
            let generate_aux_info_here = generate_aux_info_for_rule generate_aux_info r in
            Some
              (pp_kind_fun_name kind r.rule_ntr_name
               ^ pp_params r
               ^ " x = match x with\n"
               ^ String.concat ""
                   (List.map
                      (pp_kind_prod kind yo imported_pp_naming
                         generate_aux_info_here xd ts r)
                      r.rule_ps)
               ^ "\n"))

let pp_kind_defns_and_rules kind yo imported_pp_naming generate_aux_info xd ts mds rs =
  let_rec_block
    ((Auxl.option_map (pp_kind_metavar_defn kind yo xd) mds)
     @ (Auxl.option_map
          (pp_kind_rule kind yo imported_pp_naming generate_aux_info xd ts)
          rs))

let imported_pp_interface imported_pp_naming deps =
  let providers = imported_pp_providers deps in
  let pp_dep dep =
    let ty =
      (imported_pp_provider_names imported_pp_naming dep.ip_provider).ipp_public_path
      ^ "."
      ^ dep.ip_origin_name
    in
    String.concat ""
      (List.map
         (fun kind ->
            "    val "
            ^ pp_kind_fun_name kind dep.ip_origin_name
            ^ " : "
            ^ ty
            ^ " -> PPrint.document\n")
         (enabled_pp_kinds ()))
  in
  "module type Imported_pp = sig\n"
  ^ String.concat ""
      (List.map (fun provider ->
         let binder_name =
           (imported_pp_provider_names imported_pp_naming provider).ipp_binder_name
         in
         let provider_deps =
           List.filter (fun dep -> dep.ip_provider = provider) deps
         in
         "  module " ^ binder_name ^ " : sig\n"
         ^ String.concat "" (List.map pp_dep provider_deps)
         ^ "  end\n"
      ) providers)
  ^ "end\n\n"

let wrap_imported_pp_body imported_pp_naming deps body =
  if deps = [] then
    body
  else
    imported_pp_interface imported_pp_naming deps
    ^ "module Make (Imported : Imported_pp) = struct\n"
    ^ body
    ^ "\nend\n"

let string_has_suffix s suffix =
  let s_len = String.length s in
  let suffix_len = String.length suffix in
  s_len >= suffix_len
  && String.sub s (s_len - suffix_len) suffix_len = suffix

let menhir_related_filename output_file replacement_suffix =
  if string_has_suffix output_file "_parser.mly" then
    String.sub output_file 0 (String.length output_file - String.length "_parser.mly")
    ^ replacement_suffix
  else if string_has_suffix output_file ".mly" then
    String.sub output_file 0 (String.length output_file - 4) ^ replacement_suffix
  else
    output_file ^ replacement_suffix

let menhir_lib_filename output_file =
  menhir_related_filename output_file "_lib.mly"

let menhir_legacy_parse_filename output_file =
  menhir_related_filename output_file "_parse.mly"

let remove_file_if_exists filename =
  if Sys.file_exists filename then
    try Sys.remove filename
    with Sys_error _ -> ()

let pp_menhir_generated_banner fd sources =
  Printf.fprintf fd "/* generated by Ott %s from: %s */\n" Version.n sources

let pp_menhir_parser_header fd yo imported_providers =
  output_string fd "%{\n";
  List.iter (fun provider ->
    output_string fd
      ("module "
       ^ menhir_ast_alias_name_of_source_file provider.pi_file
       ^ " = "
       ^ Import_backend.module_id (Menhir yo) provider
       ^ "\n")
  ) imported_providers;
  output_string fd ("open " ^ yo.ppm_caml_ast_module ^ "\n");
  output_string fd "%}\n\n"

let pp_menhir_rules yo imported_pp_naming wrap_action_in_provider_alias
    naming generate_aux_info xd ts rs =
  String.concat ""
    (List.map
       (pp_menhir_rule yo imported_pp_naming wrap_action_in_provider_alias
          naming generate_aux_info xd ts)
       rs)

let pp_menhir_parser_file fd sources m yo lookup imported_pp_naming
    generate_aux_info xd ts local_view parser_summary =
  let provider_cache = Hashtbl.create 8 in
  let imported_providers, provider_tokens =
    menhir_provider_closure yo xd provider_cache
      parser_summary.mrs_direct_providers
  in
  let parser_tokens =
    dedup_token_entries
      (token_entries_of_deps yo xd parser_summary.mrs_token_deps
       @ provider_tokens)
  in
  pp_menhir_generated_banner fd sources;
  pp_menhir_parser_header fd yo imported_providers;
  List.iter (pp_menhir_token fd) parser_tokens;
  Printf.fprintf fd "%%token EOF  (* added by Ott *)\n";
  output_string fd "\n";
  pp_local_menhir_embeds fd m xd lookup local_view.lbv_structure;
  output_string fd
    (pp_menhir_start_symbols yo generate_aux_info xd local_view.lbv_rules);
  output_string fd "\n\n%%\n\n";
  output_string fd (pp_menhir_start_rules yo xd ts local_view.lbv_rules);
  output_string fd
    (pp_menhir_rules yo imported_pp_naming false Menhir_root_rules
       generate_aux_info xd ts
       local_view.lbv_rules)

let pp_menhir_lib_file fd sources yo imported_pp_naming generate_aux_info xd ts
    lib_rules =
  pp_menhir_generated_banner fd sources;
  output_string fd "%%\n\n";
  output_string fd
    (pp_menhir_rules yo imported_pp_naming true Menhir_lib_rules
       generate_aux_info xd ts
       lib_rules)



(* old code to pull out precedence/assoc info*)
(*
let pp_menhir_precedences fd ts =
  let go_fun (_, (t, s, hs)) = 
    if t = "" then None else
    try match List.assoc "prec" hs with
      | [Hom_string x] ->
           Some (s, int_of_string x,
                 List.exists (fun (x,_) -> x="leftassoc") hs,
                 List.exists (fun (x,_) -> x="rightassoc") hs)
      | _ -> None
    with _ -> None
  in
  let rec group xs = match xs with
    | [] -> []
    | (s,c,l,r)::xs -> group1 [s] c l r xs
  and group1 b c l r xs = match xs with
   | (s',c',l',r')::xs when c'=c -> group1 (s'::b) c (l||l') (r||r') xs
   | _ -> (b,l,r) :: group xs in
  let ts = Auxl.option_map go_fun ts in
  let ts = List.sort (fun (_,c1,_,_) (_,c2,_,_) -> c1-c2) ts in
  let gs = group ts in
  List.iter (fun (b,l,r) -> 
    if l then output_string fd "\n%left"
    else if r then output_string fd "\n%right"
    else output_string fd "\n%nonassoc";
    List.iter (fun s -> output_char fd ' '; output_string fd s) b) gs
*)

(* output a menhir source file *)
let pp_menhir_syntaxdefn m sources _(*xd_quotiented*) sd_unquotiented lookup generate_aux_info oi = 
  let yo = match m with Menhir yo -> yo | _ -> raise (Failure "pp_menhir_systemdefn called with bad ppmode") in 
  let xd_unquotiented = sd_unquotiented.syntax in
  (* Store the syntax definition for use in rule suppression decisions *)
  yo.syntaxdefn <- Some xd_unquotiented;
  let local_view =
    local_backend_view xd_unquotiented sd_unquotiented.structure
  in
  let parser_summary =
    menhir_rule_summary yo xd_unquotiented local_view.lbv_rules
  in
  let lib_rules =
    menhir_lib_rules_of_local_view yo xd_unquotiented local_view
  in
  let imported_pp_naming =
    imported_pp_naming yo parser_summary.mrs_imported_pp_deps
  in
  validate_menhir_summary xd_unquotiented parser_summary;
  match oi with
  | (o,is)::[] ->
      let _ = Auxl.filename_check m o in
      let parser_fd = open_out o in
      let ts =
        token_entries_of_deps yo xd_unquotiented
          parser_summary.mrs_token_deps
      in
      pp_menhir_parser_file parser_fd sources m yo lookup imported_pp_naming
        generate_aux_info xd_unquotiented ts local_view parser_summary;
      close_out parser_fd;
      remove_file_if_exists (menhir_legacy_parse_filename o);
      remove_file_if_exists (menhir_related_filename o "_prelude.mly");
      remove_file_if_exists
        (if string_has_suffix o ".mly" then
           String.sub o 0 (String.length o - 4) ^ "_prelude.mly"
         else
           o ^ "_prelude.mly");
      (match lib_rules with
       | None ->
           remove_file_if_exists (menhir_lib_filename o)
       | Some lib_rules ->
           let lib_fd = open_out (menhir_lib_filename o) in
           let lib_summary =
             menhir_rule_summary yo xd_unquotiented lib_rules
           in
           let lib_ts =
             token_entries_of_deps yo xd_unquotiented lib_summary.mrs_token_deps
           in
           pp_menhir_lib_file lib_fd sources yo imported_pp_naming
             generate_aux_info xd_unquotiented lib_ts lib_rules;
           close_out lib_fd)

  | _ -> Auxl.error None "must specify only one output file in the menhir backend.\n"

(* output pp source file (should be called with quotiented syntaxdefn file) *)
let pp_pp_syntaxdefn m sources xd_quotiented xd_unquotiented xd_quotiented_unaux generate_aux_info hack_filename oi filename =
  let yo = match m with Menhir yo -> yo | _ -> raise (Failure "pp_pp_systemdefn called with bad ppmode") in 


  let filename =
    if hack_filename then    
      match oi with
      | (o,is)::[] ->
          
          (* horrid hack to make filename for pp code by removing .mly *)
          let o_pp =
            String.sub o 0 (String.length o - 4)  ^ "_pp.ml" in
          o_pp
      | _ -> Auxl.error None "must specify only one output file in the menhir backend.\n"
            
    else
       filename 
  in

  (* for aux_style_rules true, should we generate recursive functions for the ntr and ntr_aux rules, or just for the ntr rules but with an extra Ntr_aux constructor on the lhs? *)
  let xd = xd_quotiented_unaux (*if !Global_option.aux_style_rules then xd_quotiented else xd_quotiented_unaux in*) in
  let local_rs =
    List.filter (fun r -> not (Import.is_imported_loc xd.xd_imports r.rule_loc)) xd.xd_rs
  in
  let local_summary = menhir_rule_summary yo xd local_rs in
  let ts =
    token_entries_of_deps yo xd_unquotiented local_summary.mrs_token_deps
  in
  let imported_pp_deps = local_summary.mrs_imported_pp_deps in
  let imported_pp_naming = imported_pp_naming yo imported_pp_deps in
(*
  let m_ascii = Ascii { ppa_colour = false; 
		      ppa_lift_cons_prefixes = false; 
		      ppa_ugly= false; 
		      ppa_show_deps = false; 
		      ppa_show_defns = false } in
  Printf.printf "%s\n" (Grammar_pp.pp_syntaxdefn m_ascii xd);
 *)
  let fd = open_out filename in
  Printf.fprintf fd "(* generated by Ott %s from: %s *)\n" Version.n sources;
  output_string fd "open PPrint\n";
  let body =
    String.concat "\n"
      (List.map
         (fun kind ->
            pp_kind_defns_and_rules kind yo imported_pp_naming generate_aux_info
              xd ts xd.xd_mds local_rs)
         (enabled_pp_kinds ()))
  in
  output_string fd
    ("open "
     ^ (match !Global_option.caml_pp_ast_module with None -> yo.ppm_caml_ast_module | Some s -> s)
     ^ "\n\n"
     ^ wrap_imported_pp_body imported_pp_naming imported_pp_deps body);
  close_out fd;
