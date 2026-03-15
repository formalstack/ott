include ../../common.mk

OCAMLC ?= ocamlc
MENHIR ?= menhir
MENHIRFLAGS ?= --unused-precedence-levels
OCAMLFIND ?= ocamlfind ocamlc -package pprint
OCAMLLEX ?= ocamllex -q

MENHIR_GENERATED_SUFFIXES := \
  _ast.ml \
  _lib.mly \
  _parser.mly \
  _parser_pp.ml \
  _lexer.mll \
  _parser.ml \
  _parser.mli \
  _lexer.ml

MENHIR_OBJECT_SUFFIXES := \
  _ast.cmi \
  _ast.cmo \
  _parser.cmi \
  _parser.cmo \
  _parser_pp.cmi \
  _parser_pp.cmo \
  _lexer.cmi \
  _lexer.cmo

menhir_generated_files = $(foreach root,$(1),$(foreach suffix,$(MENHIR_GENERATED_SUFFIXES),$(root)$(suffix)))
menhir_generated_objects = $(foreach root,$(1),$(foreach suffix,$(MENHIR_OBJECT_SUFFIXES),$(root)$(suffix)))
menhir_root_output = $(1)_ast.ml
menhir_parser_source = $(1)_parser.mly
menhir_pp_source = $(1)_parser_pp.ml
menhir_lexer_source = $(1)_lexer.mll
menhir_lib_source = $(1)_lib.mly
menhir_ast_object = $(1)_ast.cmo
menhir_parser_object = $(1)_parser.cmo
menhir_pp_object = $(1)_parser_pp.cmo
menhir_lexer_object = $(1)_lexer.cmo
menhir_ott_outputs = $(strip $(call menhir_root_output,$(1)) $(call menhir_parser_source,$(1)) $(call menhir_pp_source,$(1)) $(call menhir_lexer_source,$(1)) $(2))

define assert_files_exist
	@for f in $(1); do \
	  if [ -f "$$f" ]; then \
	    :; \
	  else \
	    echo "Expected file not found: $$f." >&2; \
	    exit 1; \
	  fi; \
	done
endef

define assert_files_absent
	@for f in $(1); do \
	  if [ -e "$$f" ]; then \
	    echo "Unexpected file present: $$f." >&2; \
	    exit 1; \
	  fi; \
	done
endef

define menhir_root_rule_with_input
$(call menhir_root_output,$(1)): $(OTT_BIN) $(2) $(3)
	rm -f $(call menhir_generated_files,$(1))
	$$(OTT) -i $(2) -o $(1)_ast.ml -o $(1)_parser.mly -o $(1)_lexer.mll
	$$(call assert_files_exist,$$(call menhir_ott_outputs,$(1),$(4)))
endef

define menhir_root_rule
$(call menhir_root_rule_with_input,$(1),$(1).ott,$(2),$(3))
endef

define ast_objects_rule
$(call menhir_ast_object,$(1)): $(call menhir_root_output,$(1)) $(2)
	$$(call assert_files_exist,$(call menhir_root_output,$(1)))
	$$(OCAMLC) -c $(1)_ast.ml
	$$(call assert_files_exist,$(1)_ast.cmi $(1)_ast.cmo)
endef

define parser_objects_rule
$(call menhir_parser_object,$(1)): $(call menhir_root_output,$(1)) $(2)
	$$(call assert_files_exist,$(call menhir_parser_source,$(1)) $(3))
	rm -f $(1)_parser.ml $(1)_parser.mli $(1)_parser.cmi $(1)_parser.cmo
	$$(MENHIR) $$(MENHIRFLAGS) --infer --base $(1)_parser --ocamlc '$$(OCAMLC) -I .' $(1)_parser.mly $(3)
	$$(call assert_files_exist,$(1)_parser.ml $(1)_parser.mli)
	$$(OCAMLC) -c $(1)_parser.mli
	$$(OCAMLC) -c $(1)_parser.ml
	$$(call assert_files_exist,$(1)_parser.cmi $(1)_parser.cmo)
endef

define lexer_objects_rule
$(call menhir_lexer_object,$(1)): $(call menhir_root_output,$(1)) $(2)
	$$(call assert_files_exist,$(call menhir_lexer_source,$(1)))
	rm -f $(1)_lexer.ml $(1)_lexer.cmi $(1)_lexer.cmo
	$$(OCAMLLEX) -o $(1)_lexer.ml $(1)_lexer.mll
	$$(call assert_files_exist,$(1)_lexer.ml)
	$$(OCAMLC) -c $(1)_lexer.ml
	$$(call assert_files_exist,$(1)_lexer.cmi $(1)_lexer.cmo)
endef

define pp_objects_rule
$(call menhir_pp_object,$(1)): $(call menhir_root_output,$(1)) $(2)
	$$(call assert_files_exist,$(call menhir_pp_source,$(1)))
	rm -f $(1)_parser_pp.cmi $(1)_parser_pp.cmo
	$$(OCAMLFIND) -I . -c $(1)_parser_pp.ml
	$$(call assert_files_exist,$(1)_parser_pp.cmi $(1)_parser_pp.cmo)
endef

define menhir_compile_root
$(eval $(call ast_objects_rule,$(1),$(2)))
$(eval $(call parser_objects_rule,$(1),$(call menhir_ast_object,$(1)) $(2),$(3)))
$(eval $(call pp_objects_rule,$(1),$(call menhir_ast_object,$(1)) $(2) $(4)))
$(eval $(call lexer_objects_rule,$(1),$(call menhir_parser_object,$(1))))
endef

define clean_rule
clean:
	@rm -f $(strip $(1)) *.cmx *.o
endef
