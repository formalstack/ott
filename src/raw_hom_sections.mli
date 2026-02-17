(** Desugar [homs ...] sections into attached hom/bindspec lists.

    The core typechecker treats hom sections as syntax sugar that appends
    homomorphisms (and, for productions, bindspecs) onto existing declarations.
    The import resolver must perform the same desugaring for imported modules,
    because it drops raw [Raw_item_hs] items.

    This module centralizes the desugaring so import resolution and typechecking
    stay consistent. *)

val apply_to_raw_syntaxdefn : Types.raw_syntaxdefn -> Types.raw_syntaxdefn
(** Apply hom sections recorded in [raw_sd_hss] and clear that field. *)

val apply_to_items : Types.raw_item list -> Types.raw_item list
(** Apply all [Raw_item_hs] hom sections in [items], drop the [Raw_item_hs]
    entries, and return the updated items. *)

