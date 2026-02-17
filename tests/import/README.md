# Import Tests

The import test suite is organized by property, not by individual regression.

## Suites

- `parser_boundary`
  Tests parser and lexer boundaries around import syntax.
- `resolution_scope`
  Tests import resolution, dependency closure, explicit scope, transitive visibility, and nested-module validation.
- `surface_name_resolution`
  Tests how imported names resolve across primaries, synonyms, and explicit renames.
- `rocq_qualification`
  Tests Rocq backend qualification and imported-name hygiene.
- `per_input_scope`
  Tests per-importing-file scoping in multi-input runs.
- `tex_surface_names`
  Tests TeX rendering of imported bodies in the importer surface namespace.
- `tex_link_hygiene`
  Tests TeX inlining/linking hygiene, support structure preservation, and regression safety.

## Oracle Policy

- Ott is run with `-signal_parse_errors true` throughout this suite.
- Rocq scenarios check emitted output, compile the generated `.v`, and compile a handwritten `check.v`.
- TeX scenarios keep explicit visible-output checks and the shared no-internal-name assertions.
- Scenarios that exercise invalid input pass only when the command is rejected with the intended file and diagnostic.

## Coverage Migration

- `tests/import/defnlang_import_keyword` -> `tests/import/parser_boundary/defnlang_import_keyword`
- `tests/import/basic` -> `tests/import/resolution_scope/basic`
- `tests/import/direct` -> `tests/import/resolution_scope/direct`
- `tests/import/collision` -> `tests/import/resolution_scope/collision`
- `tests/import/diamond` -> `tests/import/resolution_scope/diamond`
- `tests/import/negative_transitive_scope` -> `tests/import/resolution_scope/negative_transitive_scope`
- `tests/import/negative_nested_scope_validation` -> `tests/import/resolution_scope/negative_nested_scope_validation`
- `tests/import/negative_judg_transitive` -> `tests/import/resolution_scope/negative_judg_transitive`
- `tests/import/rename_synonym` -> `tests/import/surface_name_resolution/rename_synonym`
- `tests/import/rename_existing_synonym` -> `tests/import/surface_name_resolution/rename_existing_synonym`
- `tests/import/synonym_ambig` -> `tests/import/surface_name_resolution/synonym_ambig`
- `tests/import/embedded_syntax_qual` -> `tests/import/rocq_qualification/embedded_syntax_qual`
- `tests/import/embedded_syntax_hom_qual` -> `tests/import/rocq_qualification/embedded_syntax_hom_qual`
- `tests/import/hom_binder_not_qualified` -> `tests/import/rocq_qualification/hom_binder_not_qualified`
- `tests/import/hidden_ctor_qual` -> `tests/import/rocq_qualification/hidden_ctor_qual`
- `tests/import/listtuple_proj` -> `tests/import/rocq_qualification/listtuple_proj`
- `tests/import/multiinput` -> `tests/import/per_input_scope/multiinput`
- `tests/import/multiinput_same_module_backend_ids` -> `tests/import/per_input_scope/multiinput_same_module_backend_ids`
- `tests/import/tex_multiinput_root_scope` -> `tests/import/per_input_scope/tex_multiinput_root_scope`
- `tests/import/tex_multiinput_metavar_root_scope` -> `tests/import/per_input_scope/tex_multiinput_metavar_root_scope`
- `tests/import/tex_kwG_import` -> `tests/import/tex_surface_names/tex_kwG_import`
- `tests/import/tex_link_imported_synonym_root` -> `tests/import/tex_surface_names/tex_link_imported_synonym_root`
- `tests/import/tex_synonym_rename_collision` -> `tests/import/tex_surface_names/tex_synonym_rename_collision`
- `tests/import/tex_inline` -> `tests/import/tex_link_hygiene/tex_inline`
- `tests/import/tex_import_meta_hom_missing` -> `tests/import/tex_link_hygiene/tex_import_meta_hom_missing`
- `tests/import/tex_pp_inconsistent_structure` -> `tests/import/tex_link_hygiene/tex_pp_inconsistent_structure`
- `tests/import/tex_transitive_collision` -> `tests/import/tex_link_hygiene/tex_transitive_collision`
- `tests/import/tex_transitive_subst_shadow` -> `tests/import/tex_link_hygiene/tex_transitive_subst_shadow`
