(** Import system — module loading, dependency analysis, flattening *)

(** Resolve all import directives in the document.
    Returns the expanded document (with imported items injected)
    and the import context for output qualification. *)
val resolve_imports :
  base_dirs:string list ->
  search_paths:string list ->
  keep_imported_defn_bodies:bool ->
  Types.raw_item list list ->
  Types.raw_item list list * Types.import_context

(** List of all imported file paths (for source_filenames). *)
val imported_files : unit -> string list

(** Test whether a loc belongs to an imported file. *)
val is_imported_loc : Types.import_context -> Types.loc -> bool

(** Test whether a filename is an imported file. *)
val is_imported_file : Types.import_context -> string -> bool

(** Return the original (source module) name for imported items,
    or the name as-is for local items. *)
val display_name : Types.import_context -> string -> string

(** Run a computation with the current output source-file context. *)
val with_current_source_file : string option -> (unit -> 'a) -> 'a

(** Access the cached, expanded raw items for a loaded module file.
    The path must be the canonical absolute path returned by import resolution
    for a provider module (e.g. [provider.pi_file] from [direct_providers]). *)
val get_loaded_module_items : string -> Types.raw_item list option

(** Look up structured information for an imported/local binding name. *)
val lookup_binding : Types.import_context -> string -> Types.binding_info option

(** Look up binding info, preferring the current output source-file context. *)
val lookup_binding_for_current_source :
  Types.import_context -> string -> Types.binding_info option

(** Look up binding info for a specific importing source file. *)
val lookup_binding_in_source :
  Types.import_context ->
  source_file:string ->
  string ->
  Types.binding_info option

(** Look up structured provider metadata by module name. *)
val lookup_provider : Types.import_context -> string -> Types.provider_info option

(** Look up provider info, preferring the current output source-file context. *)
val lookup_provider_for_current_source :
  Types.import_context -> string -> Types.provider_info option

(** Look up provider info for a specific importing source file. *)
val lookup_provider_in_source :
  Types.import_context ->
  source_file:string ->
  string ->
  Types.provider_info option

(** Look up structured per-source-file scope information. *)
val lookup_scope_info : Types.import_context -> string -> Types.scope_info option

(** Direct providers in stable import order. *)
val direct_providers : Types.import_context -> Types.provider_info list

(** Direct providers reachable from a particular set of importing files. *)
val direct_providers_for_source_files :
  Types.import_context -> string list -> Types.provider_info list

(** Structured provider-root links for one importing file/module pair. *)
val root_links_in_source :
  Types.import_context ->
  source_file:string ->
  provider_module:string ->
  Types.import_root_link list

(** Importing source files that bind a given local imported name. *)
val source_files_for_binding :
  Types.import_context -> string -> string list

(** Extend an import context with a synthetic binding if it is absent. *)
val add_binding_if_absent :
  Types.import_context ->
  Types.binding_addition ->
  unit
