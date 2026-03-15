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
- `ocaml_modules`
  Tests that OCaml module output stays separate-compiled, qualifies imported
  references in structured output positions, and leaves raw OCaml snippets
  untouched.
- `menhir`
  Menhir-specific import tests. The sub-suites in `menhir/` cover the
  import-frontier rejection of import-block `{{ menhir ... }}`, `{{ ocaml ... }}`
  path-only imports, synthesized PP binder hygiene, declaration policy, lexer
  policy, multi-file composition contract, `{{ menhir-public }}` visibility
  rule, provider-stable export names, and physical provider-identity stability
  across path aliases.
  See `menhir/README.md` for the property-level breakdown.
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
- Menhir scenarios build a root `*_parser.mly` together with the deduplicated
  transitive set of provider `*_lib.mly` files, and check that no legacy
  `*_parse.mly` artifact remains.
