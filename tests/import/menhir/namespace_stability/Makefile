include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,left right main) \
  $(call menhir_generated_objects,left right main)

.PHONY: all clean exports

all: exports

$(eval $(call menhir_root_rule,left,,left_lib.mly))
$(eval $(call menhir_root_rule,right,,right_lib.mly))
$(eval $(call menhir_root_rule,main,left.ott right.ott))
$(eval $(call menhir_compile_root,left,,,))
$(eval $(call menhir_compile_root,right,,,))
$(eval $(call menhir_compile_root,main,$(call menhir_ast_object,left) $(call menhir_ast_object,right),$(call menhir_lib_source,left) $(call menhir_lib_source,right),))

exports: $(call menhir_parser_object,main)
	$(call assert_exists,left_lib.mly)
	$(call assert_exists,right_lib.mly)
	@left_export=$$(awk '/^%public / { print $$2; exit }' left_lib.mly); \
	right_export=$$(awk '/^%public / { print $$2; exit }' right_lib.mly); \
	left_export=$${left_export%:}; \
	right_export=$${right_export%:}; \
	if [ -z "$$left_export" ] || [ -z "$$right_export" ]; then \
	  echo "Expected %public exports in provider lib files." >&2; \
	  exit 1; \
	fi; \
	if [ "$$left_export" = "$$right_export" ]; then \
	  echo "Provider Menhir exports collided: $$left_export" >&2; \
	  exit 1; \
	fi; \
	rg -F -q -- "$$left_export" main_parser.mly || { echo "Missing $$left_export in main_parser.mly" >&2; exit 1; }; \
	rg -F -q -- "$$right_export" main_parser.mly || { echo "Missing $$right_export in main_parser.mly" >&2; exit 1; }
	$(call assert_absent_re,left_lib.mly,"^term:")
	$(call assert_absent_re,right_lib.mly,"^term:")
	$(call assert_files_absent,left_parse.mly right_parse.mly main_parse.mly)
	$(call assert_no_internal_import_names,main_parser.mly)

$(eval $(call clean_rule,$(CLEAN_FILES)))
