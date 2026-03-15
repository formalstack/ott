include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main)

.PHONY: all clean artifact_contract

all: artifact_contract

$(eval $(call menhir_root_rule,lib,,lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call menhir_compile_root,lib,,,))
$(eval $(call menhir_compile_root,main,$(call menhir_ast_object,lib),$(call menhir_lib_source,lib),))

artifact_contract: $(call menhir_pp_object,lib) $(call menhir_parser_object,main) $(call menhir_lexer_object,main) $(call menhir_pp_object,main)
	$(call assert_files_exist,lib_ast.ml lib_lib.mly lib_parser.mly lib_parser_pp.ml lib_lexer.mll main_ast.ml main_parser.mly main_parser_pp.ml main_lexer.mll)
	$(call assert_files_absent,lib_parse.mly main_parse.mly main_lib.mly)
	$(call assert_contains,lib_lib.mly,"%public ")
	$(call assert_absent,lib_parser.mly,"%public ")
	$(call assert_absent,main_parser.mly,"%public ")
	$(call assert_contains,main_parser.mly,"wrapped = wrapped EOF")

$(eval $(call clean_rule,$(CLEAN_FILES)))
