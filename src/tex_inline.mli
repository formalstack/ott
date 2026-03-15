(** TeX backend import inlining/linking.

    The core typechecking pipeline can treat imported defn/fundefn bodies as
    interfaces (bodies stripped). For TeX output, we want to inline the
    implementations of imported defns that are part of the selected import
    closure, without relying on unsafe string rewriting.
*)

val inline_imported_defn_bodies :
  m_tex:Types.pp_mode ->
  quotient_rules:bool ->
  generate_aux_rules:bool ->
  targets_non_tex:string list ->
  merge_fragments:bool ->
  Types.systemdefn ->
  Types.systemdefn
