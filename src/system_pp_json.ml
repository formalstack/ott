module Y = Yojson.Safe;;

open Y

let json_option (f: 'a -> Y.t) (v: 'a option) =
    match v with
    | Some v -> (f v)
    | None -> `Null

let rec json_string (s : string) =
    `String s

and json_stringset (ss : Types.StringSet.t) =
    `List (List.map (fun s -> `String s) (Types.StringSet.elements ss))

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

and json_metavar ((root, suffix) : Types.metavar) =
    `Assoc [
        ("root", `String root);
        ("suffix", json_suffix suffix)
    ]

and json_homomorphism ((n,ss) : Types.homomorphism) =
    let spec = List.map (fun hse ->
        match hse with
        | Types.Hom_string s -> `Assoc [("string", `String s)]
        | Types.Hom_index i -> `Assoc [("index", `Int i)]
        | Types.Hom_terminal t -> `Assoc [("terminal", `String t)]
        | Types.Hom_ln_free_index (ts,i) -> `Assoc [
            ("ln_free_index", `Assoc [
                ("values", `List (List.map (fun i -> `Int i) ts));
                ("value", `Int i)
            ])
        ]
    ) ss in
    `Assoc [
        ("name", `String n);
        ("spec", `List spec)
    ]

and json_homomorphisms (lh : Types.homomorphism list) =
    `List (List.map json_homomorphism lh)

and json_metavardefn (mvd : Types.metavardefn) =
    `Assoc [
        ("name", `String mvd.mvd_name);
        ("names", `List (List.map (fun (n,hs) ->
            `Assoc [ ("root", `String n); ("homs", json_homomorphisms hs) ]) mvd.mvd_names));
        ("rep", json_homomorphisms mvd.mvd_rep);
        ("indexvar", `Bool mvd.mvd_indexvar);
        ("locally_nameless", `Bool mvd.mvd_locally_nameless);
        ("phantom", `Bool mvd.mvd_phantom);
        ("loc", json_loc mvd.mvd_loc)
    ]

and json_element (e : Types.element) =
    match e with
    | Types.Lang_nonterm (r,nt) -> `Assoc [
        ("nonterm", `Assoc [
            ("root", `String r);
            ("nonterm", json_nonterm nt)
        ])
    ]
    | Types.Lang_metavar (r,mv) -> `Assoc [
        ("metavar", `Assoc [
            ("root", `String r);
            ("metavar", json_metavar mv)
        ])
    ]
    | Types.Lang_terminal (t) -> `Assoc [
        ("terminal", `String t)
    ]
    | Types.Lang_option el -> `Assoc [
        ("option", `List (List.map json_element el))
    ]
    | Types.Lang_sugaroption s -> `Assoc [
        ("sugaroption", `String s)
    ]
    | Types.Lang_list elb -> `Assoc [
        ("list", `Assoc [
            ("bound", json_option json_bound elb.elb_boundo);
            ("tm", json_option json_string elb.elb_tmo);
            ("es", `List (List.map json_element elb.elb_es))
        ])
    ]

and json_bindspec (bs : Types.bindspec) =
    `Null

and json_prod_flavour (pf : Types.prod_flavour) =
    match pf with
    | Types.Bar -> `String "bar"

and json_prod (p : Types.prod) =
    `Assoc [
        ("name", `String p.prod_name);
        ("flavour", json_prod_flavour p.prod_flavour);
        ("meta", `Bool p.prod_meta);
        ("sugar", `Bool p.prod_sugar);
        ("categories", json_stringset p.prod_categories);
        ("es", `List (List.map json_element p.prod_es));
        ("homs", json_homomorphisms p.prod_homs);
        ("disambiguate", json_option (fun (s,t) -> `List [ json_string s; json_string t ]) p.prod_disambiguate);
        ("bs", `List (List.map json_bindspec p.prod_bs));
        ("loc", json_loc p.prod_loc)
    ]

and json_rule (rule : Types.rule) =
    `Assoc [
        ("name", `String rule.rule_ntr_name);
        ("names", `List (List.map (fun (r,hs) -> `Assoc [
            ("root", `String r);
            ("homs", json_homomorphisms hs)
        ]) rule.rule_ntr_names));
        ("pn_wrapper", `String rule.rule_pn_wrapper);
        ("prods", `List (List.map json_prod rule.rule_ps));
        ("homs", json_homomorphisms rule.rule_homs);
        ("meta", `Bool rule.rule_meta);
        ("semi_meta", `Bool rule.rule_semi_meta);
        ("phantom", `Bool rule.rule_phantom);
        ("judgement", `Bool rule.rule_judgement);
        ("loc", json_loc rule.rule_loc)
    ]

and json_subrule (sr : Types.subrule) =
    `Null

and json_syntaxdefn (syn : Types.syntaxdefn) =
    `Assoc [
        ("mds", `List (List.map json_metavardefn syn.xd_mds));
        ("rules", `List (List.map json_rule syn.xd_rs));
        ("dep", `Null);
        ("srs", `List (List.map json_subrule syn.xd_srs));
        ("srd", `Null);
        ("crs", `Null);
        ("axs", `Null);
        ("sbs", `Null);
        ("fvs", `Null);
        ("embed_preamble", `List (List.map json_embedmorphism syn.xd_embed_preamble));
        ("embed", `List (List.map json_embedmorphism syn.xd_embed));
        ("isa_imports", `List (List.map json_string syn.xd_isa_imports));
        ("pas", `Null)
    ]

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
        ("homs", json_homomorphisms fd.fd_homs);
        ("loc", json_loc fd.fd_loc)
    ]

and json_fundefnclass (fdc: Types.fundefnclass) =
    `Assoc [
        ("name", `String fdc.fdc_name);
        ("homs", json_homomorphisms fdc.fdc_homs);
        ("fundefns", `List (List.map json_fundefn fdc.fdc_fundefns));
        ("loc", json_loc fdc.fdc_loc)
    ]

and json_symterm_list_item (stli : Types.symterm_list_item) =
    match stli with
    | Types.Stli_single (loc, es) -> `Assoc [
        ("single", `Assoc [
            ("loc", json_loc loc);
            ("list", `List (List.map json_symterm_element es))
        ])
    ]
    | Types.Stli_listform stlb -> `Assoc [
        ("listform", `Assoc [
            ("bound", json_bound stlb.stl_bound);
            ("elements", `List (List.map json_symterm_element stlb.stl_elements));
            ("loc", json_loc stlb.stl_loc)
        ])
    ]

and json_symterm_element (ses : Types.symterm_element) =
    match ses with
    | Ste_st (loc,st) -> `Assoc [
        ("st", `Assoc [
            ("loc", json_loc loc);
            ("st", json_symterm st)
        ])
    ]
    | Ste_metavar (loc,mvr,mv) -> `Assoc [
        ("metavar", `Assoc [
            ("loc", json_loc loc);
            ("mvr", `String mvr);
            ("mv", json_metavar mv)
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
        ("categories", json_stringset dr.drule_categories);
        ("premises", `List (List.map (fun (label,term) ->
            `Assoc [
                ("label", json_option json_string label);
                ("term", json_symterm term)
            ]
        ) dr.drule_premises));
        ("conclusion", json_symterm dr.drule_conclusion);
        ("homs", json_homomorphisms dr.drule_homs);
        ("loc", json_loc dr.drule_loc)
    ]

and json_embed_spec_el (ese : Types.embed_spec_el) =
    match ese with
    | Types.Embed_string (loc,s) -> `Assoc [
        ("string", `Assoc [
            ("loc", json_loc loc);
            ("value", json_string s)
        ])
    ]
    | Types.Embed_inner (loc,s) -> `Assoc [
        ("inner", `Assoc [
            ("loc", json_loc loc);
            ("value", json_string s)
        ])
    ]

and json_embed_spec (dr : Types.embed_spec) =
    `List (List.map json_embed_spec_el dr)

and json_embedmorphism (em : Types.embedmorphism) =
    let (loc,name,spec) = em in
    `Assoc [
        ("loc", json_loc loc);
        ("hom", json_string name);
        ("spec", json_embed_spec spec)
    ]

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
        ("homs", json_homomorphisms d.d_homs);
        ("loc", json_loc d.d_loc)
    ]

and json_defnclass (dc: Types.defnclass) =
    `Assoc [
        ("name", `String dc.dc_name);
        ("homs", json_homomorphisms dc.dc_homs);
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

and json_structure_entry (stre : Types.structure_entry) =
    (match stre with
    | Types.Struct_md mvr -> `Assoc [
        ("md", `String mvr)
    ]
    | Types.Struct_rs ntrs -> `Assoc [
        ("rs", `List (List.map json_string ntrs))
    ]
    | Types.Struct_srs ntrs -> `Assoc [
        ("srs", `List (List.map (fun (x,y) -> `List [`String x; `String y]) ntrs))
    ]
    | Types.Struct_crs ntrs -> `Assoc [
        ("crs", `List (List.map (fun (x,y,z) -> `List [`String x; `String y; `String z]) ntrs))
    ]
    | Types.Struct_axs auxfns -> `Assoc [
        ("axs", `List (List.map json_string auxfns))
    ]
    | Types.Struct_sbs sbs -> `Assoc [
        ("sbs", `Null)
    ]
    | Types.Struct_fvs fvs -> `Assoc [
        ("fvs", `Null)
    ]
    | Types.Struct_embed embed -> `Assoc [
        ("embed", `Null)
    ]
    | Types.Struct_fun_or_defnclass name -> `Assoc [
        ("fun_or_defnclass", `String name)
    ]
    )

and json_structure (strs : Types.structure) =
    `List (List.map (fun (name, stre) -> `Assoc [
        ("name", `String name);
        ("struct", json_structure_entry stre)
    ]) strs)

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
