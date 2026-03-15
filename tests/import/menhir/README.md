# Menhir Import Tests

The Menhir import tests are grouped by backend property, not by individual
regression.

- `artifact_contract`
  A Menhir root always emits `*_parser.mly`, a provider emits `*_lib.mly` iff
  it exports `{{ menhir-public }}`, and no `*_parse.mly` artifact exists.
- `composition`
  Menhir module output composes via the root `*_parser.mly` plus transitive
  provider `*_lib.mly` files, while keeping pretty-printers functorized over
  imported providers.
- `declaration_policy`
  The root wrapper emits only local Menhir declaration embeds, while imported
  token regexps still flatten into the root lexer and precedence is expressed
  using importer-visible token names.
- `token_surface`
  The root parser may declare a conservative token superset from included
  provider libs, while the root lexer remains exact over the actually used
  imported closure.
- `transitive_libs`
  A root parser composes against the deduplicated transitive set of provider
  `*_lib.mly` files required by direct and transitive imports.
- `provider_scope_alias`
  Imported provider grammar compiles under importer-chosen multi-segment
  `{{ ocaml ... }}` paths without rewriting provider semantic actions.
- `ocaml_path_only`
  Menhir imports use only the provider `{{ ocaml ... }}` path for generated
  OCaml-qualified type positions; no separate import-block `{{ menhir ... }}`
  override exists.
- `binder_uniqueness`
  Imported Menhir PP binders are synthesized internally from provider OCaml
  paths, sanitized into valid OCaml module identifiers, and kept distinct when
  multiple imports would otherwise collide.
- `lexer_policy`
  The root lexer flattens imported token regexps but keeps `{{ lex-comment }}`
  local-only and rejects conflicting local markers.
- `public_visibility`
  Unsuppressed local use of imported nonterminals requires `{{ menhir-public }}`,
  while suppressed-only use does not.
- `namespace_stability`
  Provider `%public` export aliases are provider-stable, so two imported
  providers with the same surface rule name do not collide.
- `provider_namespace_identity`
  Provider `%public` export aliases are keyed by the resolver's physical file
  identity, so standalone provider emission and importer references agree even
  when the same file is reached through a symlink alias.
