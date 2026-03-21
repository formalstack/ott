(**************************************************************************)
(*                                   Ott                                  *)
(*                                                                        *)
(*  Import naming & hygiene scheme                                        *)
(**************************************************************************)

open Types

let source_file_of_loc ~(default : string) (l : loc) : string =
  match l with
  | [] -> default
  | loc1 :: _ -> loc1.Location.loc_start.Lexing.pos_fname

let tag_of_source_file (fn : string) : string =
  let h = Digest.to_hex (Digest.string fn) in
  String.sub h 0 8

let is_synthesized_root (n : string) : bool =
  (* These names are treated as fixed internal nonterminal roots by the
     typechecker/defn parser and must never be internalized. *)
  n = "formula" || n = "judgement" || n = "terminals" || n = "user_syntax"

let is_internal_wrapper (w : string) : bool =
  let pref = "__ott_mod_" in
  let lw = String.length w and lp = String.length pref in
  lw >= lp && String.sub w 0 lp = pref

let is_internal_transitive (n : string) : bool =
  let has_pref pref =
    let ln = String.length n and lp = String.length pref in
    ln >= lp && String.sub n 0 lp = pref
  in
  has_pref "__ott_trans_" || has_pref "otttransiv"

let internalize_name ~(src : string) ~(is_indexvar : string -> bool) ~(name : string) : string =
  let tag = tag_of_source_file src in
  if is_indexvar name then
    (* Indexvars appear in suffix positions (e.g. vi, v1..vn). Suffix lexing
       treats '_' as punctuation (Auxl.issuffixpunct), so internal names for
       indexvars must not contain underscores. *)
    "otttransiv" ^ tag ^ name
  else
    "__ott_trans_" ^ tag ^ "_" ^ name

let wrapper_prefix_for_module ~(module_name : string) : string =
  "__ott_mod_" ^ tag_of_source_file ("import:" ^ module_name) ^ "_"

(* ------------------------------------------------------------------ *)
(* Inverse mapping for display                                          *)
(* ------------------------------------------------------------------ *)

let is_hex_char = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false

let has_hex8_at (s : string) (i : int) : bool =
  let len = String.length s in
  i >= 0 && i + 8 <= len
  && (let ok = ref true in
      for j = i to i + 7 do
        if not (is_hex_char s.[j]) then ok := false
      done;
      !ok)

let strip_internal_token ~(pref : string) ~(expects_underscore : bool)
    (n : string) : string option =
  let lp = String.length pref and ln = String.length n in
  if ln < lp + 8 then None
  else if String.sub n 0 lp <> pref then None
  else if not (has_hex8_at n lp) then None
  else
    let j = lp + 8 in
    if expects_underscore then
      if ln >= j + 1 && n.[j] = '_' then
        Some (String.sub n (j + 1) (ln - (j + 1)))
      else None
    else
      Some (String.sub n j (ln - j))

let strip_module_wrapper_prefix (n : string) : string option =
  strip_internal_token ~pref:"__ott_mod_" ~expects_underscore:true n

let surface_of_internal_token (n : string) : string option =
  let strip_internal pref ~expects_underscore =
    strip_internal_token ~pref ~expects_underscore n
  in
  match strip_internal "__ott_trans_" ~expects_underscore:true with
  | Some s -> Some s
  | None ->
    (match strip_module_wrapper_prefix n with
     | Some s -> Some s
     | None ->
       strip_internal "otttransiv" ~expects_underscore:false)
