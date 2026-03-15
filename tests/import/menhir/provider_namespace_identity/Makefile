include ../common.mk

ALIAS := lib_alias.ott

CLEAN_FILES := \
  $(ALIAS) \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main)

.PHONY: all clean identity

all: identity

$(ALIAS): lib.ott
	ln -s -f lib.ott $(ALIAS)

$(eval $(call menhir_root_rule_with_input,lib,./$(ALIAS),lib.ott $(ALIAS),lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call menhir_compile_root,lib,,,))
$(eval $(call menhir_compile_root,main,$(call menhir_ast_object,lib),$(call menhir_lib_source,lib),))

identity: $(call menhir_parser_object,main)
	$(call assert_exists,lib_lib.mly)
	@provider_export=$$(awk '/^%public / { print $$2; exit }' lib_lib.mly); \
	provider_export=$${provider_export%:}; \
	if [ -z "$$provider_export" ]; then \
	  echo "Expected %public export in lib_lib.mly." >&2; \
	  exit 1; \
	fi; \
	rg -F -q -- "$$provider_export" main_parser.mly || { \
	  echo "Expected importer to reference provider export $$provider_export." >&2; \
	  exit 1; \
	}
	$(call assert_files_absent,lib_parse.mly main_parse.mly)

$(eval $(call clean_rule,$(CLEAN_FILES)))
