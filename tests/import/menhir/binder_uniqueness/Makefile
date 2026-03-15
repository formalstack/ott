include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,left right main) \
  $(call menhir_generated_objects,left right main) \
  foo.cmi foo.cmo foo_bar.cmi foo_bar.cmo

.PHONY: all clean binder_uniqueness

all: binder_uniqueness

$(eval $(call menhir_root_rule,left,))
$(eval $(call menhir_root_rule,right,))
$(eval $(call menhir_root_rule,main,left.ott right.ott))
$(eval $(call ast_objects_rule,left,))
$(eval $(call ast_objects_rule,right,))

foo.cmo: foo.ml $(call menhir_ast_object,left)
	$(OCAMLC) -c foo.ml

foo_bar.cmo: foo_bar.ml $(call menhir_ast_object,right)
	$(OCAMLC) -c foo_bar.ml

binder_uniqueness: $(call menhir_root_output,main) $(call menhir_ast_object,left) $(call menhir_ast_object,right) foo.cmo foo_bar.cmo
	$(call assert_contains,main_parser_pp.ml,"Foo.Bar.term")
	$(call assert_contains,main_parser_pp.ml,"Foo_bar.term")
	$(call assert_absent,main_parser_pp.ml,"Foo.Bar_pp")
	@modules=$$(awk '/^module type Imported_pp = sig$$/ { in_sig = 1; next } in_sig && /^end$$/ { exit } in_sig && /^  module / { print $$2 }' main_parser_pp.ml | sed 's/://'); \
	if [ "$$(printf '%s\n' "$$modules" | sed '/^$$/d' | wc -l | tr -d ' ')" -ne 2 ]; then \
	  echo "Expected exactly two imported PP binders." >&2; \
	  exit 1; \
	fi; \
	first=$$(printf '%s\n' "$$modules" | sed -n '1p'); \
	second=$$(printf '%s\n' "$$modules" | sed -n '2p'); \
	for binder in "$$first" "$$second"; do \
	  printf '%s\n' "$$binder" | rg -x -q "$(IDENT_RE)" || { \
	    echo "Expected imported PP binders to be valid OCaml identifiers." >&2; \
	    exit 1; \
	  }; \
	done; \
	if [ "$$first" = "$$second" ]; then \
	  echo "Expected imported PP binders to be distinct." >&2; \
	  exit 1; \
	fi
	$(call assert_no_internal_import_names,main_parser_pp.ml)
	$(OCAMLC) -c main_ast.ml
	$(OCAMLFIND) -I . -c main_parser_pp.ml

$(eval $(call clean_rule,$(CLEAN_FILES)))
