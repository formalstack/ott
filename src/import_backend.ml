open Types

let backend_key = Auxl.hom_name_for_pp_mode

let module_id_keys (m : pp_mode) : string list =
  match m with
  | Menhir _ -> ["ocaml"]
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

let provider_hom (provider : provider_info) (key : string) : string option =
  List.assoc_opt key provider.pi_homs

let render_import_stmt (m : pp_mode) (provider : provider_info) : string option =
  match m with
  | Coq _ ->
    let imported = module_id m provider in
    (match provider_hom provider "rocq-from" with
     | Some from_ -> Some ("From " ^ from_ ^ " Require " ^ imported ^ ".")
     | None -> Some ("Require " ^ imported ^ "."))
  | _ -> None

let render_import_stmts ?(source_files = []) (m : pp_mode) (ic : import_context)
  : string list =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let stmts_rev = ref [] in
  List.iter (fun provider ->
    match render_import_stmt m provider with
    | None -> ()
    | Some stmt when Hashtbl.mem seen stmt -> ()
    | Some stmt ->
      Hashtbl.replace seen stmt ();
      stmts_rev := stmt :: !stmts_rev
  ) (Import.direct_providers_for_source_files ic source_files);
  List.rev !stmts_rev
