include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main) \
  foo.cmi foo.cmo

.PHONY: all clean ocaml_path_only

all: ocaml_path_only

$(eval $(call menhir_root_rule,lib))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call ast_objects_rule,lib,))

foo.cmo: foo.ml $(call menhir_ast_object,lib)
	$(OCAMLC) -c foo.ml

ocaml_path_only: $(call menhir_root_output,main) $(call menhir_ast_object,lib) foo.cmo
	$(call assert_contains,main_parser_pp.ml,"Foo.Bar.term")
	$(call assert_absent,main_parser_pp.ml,"Foo.Bar_pp")
	$(call assert_no_internal_import_names,main_parser_pp.ml)
	$(OCAMLC) -c main_ast.ml
	$(OCAMLFIND) -I . -c main_parser_pp.ml

$(eval $(call clean_rule,$(CLEAN_FILES)))
