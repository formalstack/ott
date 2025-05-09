module Y = Yojson.Safe;;

open Y

let rec json_option (f : 'a -> Y.t) (v : 'a option) =
    match v with
    | Some v -> (f v)
    | None -> `Null

and json_string (s: string) =
    `String s

and json_pos (pos : Lexing.position) =
    `Assoc [
        ("fname", `String pos.Lexing.pos_fname);
        ("lnum", `Int pos.Lexing.pos_lnum);
        ("bol", `Int pos.Lexing.pos_bol);
        ("cnum", `Int pos.Lexing.pos_cnum);
    ]

and json_loc (loc : Types.loc) =
    `List (List.map (
        fun locr -> `Assoc [
            ("start", json_pos locr.Location.loc_start);
            ("end", json_pos locr.Location.loc_end);
        ]) loc)

and json_suffix_item (suff : Types.suffix_item) =
    match suff with
    | Types.Si_num suffixnum -> `Assoc [
        ("suffix_num", `String suffixnum)
    ]
    | Types.Si_punct punct -> `Assoc [
        ("punct", `String punct)
    ]
    | Types.Si_var (var,offset) -> `Assoc [
        ("var", `Assoc [
            ("var", `String var);
            ("offset", `Int offset)
        ])
    ]
    | Types.Si_index ind -> `Assoc [
        ("index", `Int ind)
    ]

and json_suffix (suff: Types.suffix) = `List (List.map json_suffix_item suff)

and json_bound (bound : Types.bound) =
    match bound with
    | Types.Bound_dotform bd -> `Assoc [
        ("dotform", `Assoc [
            ("lower", json_suffix_item bd.bd_lower);
            ("upper", json_suffix_item bd.bd_upper);
            ("length", `Int bd.bd_length)
        ])
    ]
    | Types.Bound_comp bc -> `Assoc [
        ("comp", `String bc.bc_variable)
    ]
    | Types.Bound_comp_u bcu -> `Assoc [
        ("comp_u", `Assoc [
            ("variable", `String bcu.bcu_variable);
            ("upper", json_suffix_item bcu.bcu_upper)
        ])
    ]
    | Types.Bound_comp_lu bclu -> `Assoc [
        ("comp_lu", `Assoc [
            ("variable", `String bclu.bclu_variable);
            ("lower", json_suffix_item bclu.bclu_lower);
            ("upper", json_suffix_item bclu.bclu_upper);
            ("length", `Int bclu.bclu_length)
        ])
    ]

and json_nonterm ((root, suffix) : Types.nonterm) =
    `Assoc [
        ("root", `String root);
        ("suffix", json_suffix suffix)
    ]

and json_homomorphism ((n,ss) : Types.homomorphism) =
    let spec = List.map (fun hse ->
        match hse with
        | Types.Hom_string s -> `Assoc [("t", `String "string"); ("value", `String s)]
        | Types.Hom_index i -> `Assoc [("t", `String "index"); ("value", `Int i)]
        | Types.Hom_terminal t -> `Assoc [("t", `String "terminal"); ("value", `String t)]
        | Types.Hom_ln_free_index (ts,i) -> `Assoc [
            ("t", `String "ln_free_index");
            ("values", `List (List.map (fun i -> `Int i) ts));
            ("value", `Int i)
        ]
    ) ss in
    `Assoc [
        ("name", `String n);
        ("spec", `List spec)
    ]

and json_syntaxdefn (syn : Types.syntaxdefn) =
    `Null

and json_funclause (fc: Types.funclause) =
    `Assoc [
        ("lhs", json_symterm fc.fc_lhs);
        ("rhs", json_symterm fc.fc_rhs);
        ("loc", json_loc fc.fc_loc)
    ]

and json_fundefn (fd: Types.fundefn) =
    `Assoc [
        ("name", `String fd.fd_name);
        ("form", json_symterm fd.fd_form);
        ("result", `String fd.fd_result);
        ("result_type", `String fd.fd_result_type);
        ("pn_wrapper", `String fd.fd_pn_wrapper);
        ("clauses", `List (List.map json_funclause fd.fd_clauses));
        ("homs", `List (List.map json_homomorphism fd.fd_homs));
        ("loc", json_loc fd.fd_loc)
    ]

and json_fundefnclass (fdc: Types.fundefnclass) =
    `Assoc [
        ("name", `String fdc.fdc_name);
        ("homs", `List (List.map json_homomorphism fdc.fdc_homs));
        ("fundefns", `List (List.map json_fundefn fdc.fdc_fundefns));
        ("loc", json_loc fdc.fdc_loc)
    ]

and json_symterm_list_item (stli : Types.symterm_list_item) =
    match stli with
    | Types.Stli_single (loc, es) -> `Assoc [
        ("loc", json_loc loc);
        ("list", `List (List.map json_symterm_element es))
    ]
    | Types.Stli_listform stlb -> `Assoc [
        ("bound", json_bound stlb.stl_bound);
        ("elements", `List (List.map json_symterm_element stlb.stl_elements));
        ("loc", json_loc stlb.stl_loc)
    ]

and json_symterm_element (ses : Types.symterm_element) =
    match ses with
    | Ste_st (loc,st) -> `Assoc [
        ("st", `Assoc [
            ("loc", json_loc loc);
            ("st", json_symterm st)
        ])
    ]
    | Ste_metavar (loc,mvr,(mvr2,suff)) -> `Assoc [
        ("metavar", `Assoc [
            ("loc", json_loc loc);
            ("mvr", `String mvr);
            ("mv", `Assoc [
                ("mvr", `String mvr2);
                ("suffix", json_suffix suff)
            ])
        ])
    ]
    | Ste_var (loc,mvr,v) -> `Assoc [
        ("var", `Assoc [
            ("loc", json_loc loc);
            ("mvr", `String mvr);
            ("var", `String v)
        ])
    ]
    | Ste_list (loc,stlis) -> `Assoc [
        ("list", `Assoc [
            ("loc", json_loc loc);
            ("symterms", `List (List.map json_symterm_list_item stlis))
        ])
    ]

and json_symterm_node_body (body : Types.symterm_node_body) =
    `Assoc [
        ("rule_ntr_name", `String (body.st_rule_ntr_name));
        ("prod_name", `String (body.st_prod_name));
        ("es", `List (List.map json_symterm_element body.st_es));
        ("loc", json_loc body.st_loc)
    ]

and json_symterm (st : Types.symterm) =
    match st with
    | Types.St_node (loc,body) -> `Assoc [
        ("node", `Assoc [
            ("loc", json_loc loc);
            ("body", json_symterm_node_body body)
        ])
    ]
    | Types.St_nonterm (loc,root,nt) -> `Assoc [
        ("nonterm", `Assoc [
            ("loc", json_loc loc);
            ("root", `String root);
            ("nonterm", json_nonterm nt)
        ])
    ]
    | Types.St_nontermsub (loc, lower, top, nt) -> `Assoc [
        ("nontermsub", `Assoc [
            ("loc", json_loc loc);
            ("lower", `String lower);
            ("top", `String top);
            ("nonterm", json_nonterm nt)
        ])
    ]
    | Types.St_uninterpreted (loc, s) -> `Assoc [
        ("uninterpreted", `Assoc [
            ("loc", json_loc loc);
            ("value", `String s)
        ])
    ]

and json_drule (dr : Types.drule) =
    `Assoc [
        ("name", `String dr.drule_name);
        ("categories", `List (List.map (json_string) (Types.StringSet.elements dr.drule_categories)));
        ("premises", `List (List.map (fun (label,term) ->
            `Assoc [
                ("label", json_option (fun s -> `String s) label);
                ("term", json_symterm term)
            ]
        ) dr.drule_premises));
        ("conclusion", json_symterm dr.drule_conclusion);
        ("homs", `List (List.map json_homomorphism dr.drule_homs));
        ("loc", json_loc dr.drule_loc)
    ]

and json_embed_spec (dr : Types.embed_spec) =
    `Null

and json_processed_semiraw_rule (psrr : Types.processed_semiraw_rule) =
    match psrr with
    | Types.PSR_Rule drule -> `Assoc [
        ("rule", json_drule drule)
    ]
    | Types.PSR_Defncom embed_spec -> `Assoc [
        ("defncom", json_embed_spec embed_spec)
    ]

and json_defn (d: Types.defn) =
    `Assoc [
        ("name", `String d.d_name);
        ("form", json_symterm d.d_form);
        ("wrapper", `String d.d_wrapper);
        ("rules", `List (List.map json_processed_semiraw_rule d.d_rules));
        ("homs", `List (List.map json_homomorphism d.d_homs));
        ("loc", json_loc d.d_loc)
    ]

and json_defnclass (dc: Types.defnclass) =
    `Assoc [
        ("name", `String dc.dc_name);
        ("homs", `List (List.map json_homomorphism dc.dc_homs));
        ("language_names", `List (List.map (fun s -> `String s) dc.dc_language_names));
        ("wrapper", `String dc.dc_wrapper);
        ("defns", `List (List.map json_defn dc.dc_defns));
        ("loc", json_loc dc.dc_loc);
    ]

and json_fun_or_reln_defnclass (frdc : Types.fun_or_reln_defnclass) =
    match frdc with
    | Types.FDC fdc -> `Assoc [
        ("fundefnclass", json_fundefnclass fdc)
    ]
    | Types.RDC rdc -> `Assoc [
        ("defnclass", json_defnclass rdc)
    ]

and json_relationsdefn (rd : Types.relationsdefn) =
    `List (List.map json_fun_or_reln_defnclass rd)

and json_structure (st : Types.structure) =
    `Null

and json_systemdefn (pp : Types.pp_mode) (sd : Types.systemdefn) (lookup : Types.made_parser) =
    `Assoc [
        ("syntax", json_syntaxdefn sd.syntax);
        ("relations", json_relationsdefn sd.relations);
        ("structure", json_structure sd.structure);
        ("sources", `String sd.sources)
    ]

let pp_systemdefn_io (pp : Types.pp_mode) (sd : Types.systemdefn) (lookup : Types.made_parser) (outfn : string) (merge: bool) =
    let fd = open_out outfn in
    Y.to_channel fd (json_systemdefn pp sd lookup);
    close_out fd
