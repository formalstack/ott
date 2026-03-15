include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib) \
  $(call menhir_generated_objects,lib) \
  $(call menhir_generated_files,missing suppressed) \
  $(call menhir_generated_objects,missing suppressed) \
  missing.out

.PHONY: all clean missing_public suppressed_use

all: missing_public suppressed_use

$(eval $(call menhir_root_rule,lib))
$(eval $(call menhir_root_rule,suppressed,lib.ott))
$(eval $(call menhir_compile_root,lib,,,))
$(eval $(call menhir_compile_root,suppressed,$(call menhir_ast_object,lib),,))

missing_public:
	$(call assert_command_fails,$(OTT) -i missing.ott -o missing_ast.ml -o missing_parser.mly -o missing_lexer.mll,missing.out,)
	$(call assert_contains,missing.out,"menhir-public")
	$(call assert_contains,missing.out,"term")

suppressed_use: $(call menhir_parser_object,suppressed) $(call menhir_lexer_object,suppressed)
	$(call assert_contains,suppressed_parser.mly,"consumer_start:")
	$(call assert_absent,suppressed_parser.mly,"wrapped:")
	$(call assert_files_absent,suppressed_parse.mly)

$(eval $(call clean_rule,$(CLEAN_FILES)))
