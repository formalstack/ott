open Types

(** Provider-driven renaming + provision of provider productions, for linking
    one module's compiled bodies into another module's namespace. *)

type t

val build :
  importer_ic:import_context ->
  importer_source_file:string ->
  provider_module:string ->
  provider_file:string ->
  provider_syntax:syntaxdefn ->
  t

val binding_additions : t -> Types.binding_info list

val rename_symterm : t -> symterm -> symterm

val provider_by_local_prod : t -> (string, rule * prod) Hashtbl.t

val provider_mvd_by_local_root : t -> (string, metavardefn) Hashtbl.t
