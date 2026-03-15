include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main)

.PHONY: all clean provider_ocaml_sugar

all: provider_ocaml_sugar

$(eval $(call menhir_root_rule,lib,,lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call ast_objects_rule,lib,))
$(eval $(call ast_objects_rule,main,$(call menhir_ast_object,lib)))
$(eval $(call parser_objects_rule,main,$(call menhir_ast_object,lib) $(call menhir_ast_object,main),$(call menhir_lib_source,lib)))
$(eval $(call lexer_objects_rule,main,$(call menhir_parser_object,main)))

provider_ocaml_sugar: $(call menhir_lexer_object,main)
	$(call assert_contains,lib_lib.mly,"(*Case 1*) term")
	$(call assert_contains,main_lexer.mll,"IDENT")

$(eval $(call clean_rule,$(CLEAN_FILES)))
