open Types

(** Shared backend-facing import policy. *)

val backend_key : pp_mode -> string

val qualifier_sep : pp_mode -> string

val module_id : pp_mode -> provider_info -> string

val render_import_stmt : pp_mode -> provider_info -> string option

val render_import_stmts :
  ?source_files:string list ->
  pp_mode ->
  import_context ->
  string list
