(**************************************************************************)
(*                                   Ott                                  *)
(*                                                                        *)
(*        Peter Sewell, Computer Laboratory, University of Cambridge      *)
(*      Francesco Zappa Nardelli, Moscova project, INRIA Rocquencourt     *)
(*                                                                        *)
(*  Copyright 2005-2010                                                   *)
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

open Types;;

let rec embed_strings res el = match el with
  | [] -> res
  | Embed_string (l,s) :: el ->
      embed_strings (List.rev_append (Str.split (Str.regexp "[ \t]+") s) res) el
  | Embed_inner _ :: el -> embed_strings res el

let rec pp_embeds_with write m xd lookup el =
  List.iter (pp_embedmorphism_with write m xd lookup) el

and pp_embedmorphism_with write m xd lookup (l,hn,es) =
  let pp_locs = if !Global_option.output_source_locations >=2 then Grammar_pp.pp_source_location m l else "" in
  write pp_locs;
  match (m,hn) with 
  | (Ascii ao, _) -> 
      write Grammar_pp.pp_DOUBLELEFTBRACE;
      write " ";
      write (Grammar_pp.pp_hom_name m xd hn);
      write " ";
      pp_embed_spec_with write m xd lookup es;
      write " ";
      write Grammar_pp.pp_DOUBLERIGHTBRACE;
      write "\n";
  | (Coq _, "coq") | (Coq _, "rocq") 
  | (Isa _, "isa")                       
  | (Hol _, "hol") 
  | (Lem _, "lem") 
  | (Twf _, "twf") 
  | (Tex _, "tex") 
  | (Caml _, "ocaml")
  | (Menhir _, "menhir")
  | (Lex _,  "lex") -> 
      pp_embed_spec_with write m xd lookup es;
      write "\n"
  | (Coq co, "coq-lib") | (Coq co, "rocq-lib") -> 
      let x = co.coq_library in
      x := (fst !x, embed_strings (snd !x) es)
  | (Isa io, "isa-lib") -> 
      let x = io.isa_library in
      x := (fst !x, embed_strings (snd !x) es)
  | (Coq _, _) | (Isa _, _) | (Hol _,_) | (Lem _,_) | (Twf _,_) | (Tex _,_) | (Caml _,_) | (Lex _, _) | (Menhir _, _) -> ()

and pp_embed_spec_with write m xd lookup es = 
  List.iter (pp_embed_spec_el_with write m xd lookup) es

and pp_embed_spec_el_with write m xd lookup ese = 
  match m with 
  | Ascii ao -> 
      ( match ese with
      | Embed_string (l,s) -> write s
      | Embed_inner (l,s) -> 
          write Grammar_pp.pp_DOUBLELEFTBRACKET; 
          write s;
          write Grammar_pp.pp_DOUBLERIGHTBRACKET )
  | Tex xo when (match ese with Embed_inner (_,"TEX_NAME_PREFIX")->true | _->false) -> 
      write xo.ppt_name_prefix
  | Tex _ | Coq _ | Isa _ | Hol _ | Lem _ | Twf _ | Caml _ | Lex _ | Menhir _ -> 
      ( match ese with
      | Embed_string (l,s) -> write s

      | Embed_inner (l,s) -> (* "<<<<<<"^ s^">>>>>" )  *)
          (* not taking account of possible dot forms shared over different terms *)
          let st = Term_parser.just_one_parse xd lookup "user_syntax" false l s in
          let ((de1,de2) as de,de3,pptbe) = Bounds.bound_extraction m xd l [st]  in
          write (Grammar_pp.pp_symterm m xd [] de st))

let pp_embeds fd m xd lookup el =
  let write s = output_string fd s in
  pp_embeds_with write m xd lookup el

let pp_embed_spec fd m xd lookup es =
  let write s = output_string fd s in
  pp_embed_spec_with write m xd lookup es

let string_of_embeds m xd lookup el =
  let buf = Buffer.create 256 in
  let write s = Buffer.add_string buf s in
  pp_embeds_with write m xd lookup el;
  Buffer.contents buf
