open Types

let backend_key = Auxl.hom_name_for_pp_mode

let module_id_keys (m : pp_mode) : string list =
  match m with
  | Menhir _ -> ["menhir"; "ocaml"]
  | _ -> [backend_key m]

let default_module_id (m : pp_mode) (provider : provider_info) : string =
  match m with
  | Tex _ -> provider.pi_module
  | _ -> provider.pi_default_id

let qualifier_sep = function
  | Twf _ -> "/"
  | _ -> "."

let module_id (m : pp_mode) (provider : provider_info) : string =
  let rec choose = function
    | [] -> default_module_id m provider
    | key :: keys ->
      match List.assoc_opt key provider.pi_homs with
      | Some id -> id
      | None -> choose keys
  in
  choose (module_id_keys m)

let render_import_stmt (m : pp_mode) (provider : provider_info) : string option =
  match m with
  | Coq _ -> Some ("Require " ^ module_id m provider ^ ".")
  | _ -> None

let render_import_stmts ?(source_files = []) (m : pp_mode) (ic : import_context)
  : string list =
  List.filter_map (render_import_stmt m)
    (Import.direct_providers_for_source_files ic source_files)
