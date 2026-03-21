open Types

(** Stable naming + hygiene scheme used by the import system.

    This module exists so that all phases (import resolution, post-typecheck
    linking for TeX, etc.) share exactly one naming scheme. *)

val tag_of_source_file : string -> string

val source_file_of_loc : default:string -> loc -> string

val is_synthesized_root : string -> bool

val is_internal_wrapper : string -> bool

val is_internal_transitive : string -> bool

(** Internalize a transitive-only Ott name (root or indexvar root). *)
val internalize_name : src:string -> is_indexvar:(string -> bool) -> name:string -> string

(** Wrapper prefix used to namespace injected backend identifiers for a module. *)
val wrapper_prefix_for_module : module_name:string -> string

(** Strip only the per-module import wrapper prefix, leaving any remaining
    internalization untouched. *)
val strip_module_wrapper_prefix : string -> string option

(** Inverse of the internal naming scheme, for user-facing pretty-printing.

    Operates on *tokens* (raw identifier strings) and therefore also handles
    suffix-bearing forms that arise from renaming, e.g.:
      b  -> __ott_trans_<tag>_b
      b1 -> __ott_trans_<tag>_b1

    Returns [None] if the token does not match an internal scheme pattern. *)
val surface_of_internal_token : string -> string option
