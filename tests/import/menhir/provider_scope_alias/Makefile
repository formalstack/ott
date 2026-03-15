include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main) \
  provider_ast.ml provider_ast.cmi provider_ast.cmo \
  foo.cmi foo.cmo

.PHONY: all clean provider_scope_alias

all: provider_scope_alias

$(eval $(call menhir_root_rule,lib,,lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))

provider_ast.ml: $(call menhir_root_output,lib)
	cp lib_ast.ml provider_ast.ml

provider_ast.cmo: provider_ast.ml
	$(OCAMLC) -c provider_ast.ml

foo.cmo: foo.ml provider_ast.cmo
	$(OCAMLC) -c foo.ml

$(eval $(call ast_objects_rule,main,foo.cmo))
$(eval $(call parser_objects_rule,main,$(call menhir_ast_object,main) provider_ast.cmo foo.cmo,$(call menhir_lib_source,lib)))

provider_scope_alias: $(call menhir_parser_object,main)
	$(call assert_contains,main_ast.ml,"Foo.Bar.term")
	$(call assert_no_internal_import_names,main_parser.mly)

$(eval $(call clean_rule,$(CLEAN_FILES)))
