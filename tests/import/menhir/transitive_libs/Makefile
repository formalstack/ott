include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,bottom left right main) \
  $(call menhir_generated_objects,bottom left right main)

.PHONY: all clean transitive_libs

all: transitive_libs

$(eval $(call menhir_root_rule,bottom,,bottom_lib.mly))
$(eval $(call menhir_root_rule,left,bottom.ott,left_lib.mly))
$(eval $(call menhir_root_rule,right,bottom.ott,right_lib.mly))
$(eval $(call menhir_root_rule,main,bottom.ott left.ott right.ott))
$(eval $(call ast_objects_rule,bottom,))
$(eval $(call ast_objects_rule,left,$(call menhir_ast_object,bottom)))
$(eval $(call ast_objects_rule,right,$(call menhir_ast_object,bottom)))
$(eval $(call ast_objects_rule,main,$(call menhir_ast_object,left) $(call menhir_ast_object,right)))
$(eval $(call parser_objects_rule,main,$(call menhir_ast_object,bottom) $(call menhir_ast_object,left) $(call menhir_ast_object,right) $(call menhir_ast_object,main),bottom_lib.mly left_lib.mly right_lib.mly))

transitive_libs: $(call menhir_parser_object,main)
	$(call assert_exists,bottom_lib.mly)
	$(call assert_exists,left_lib.mly)
	$(call assert_exists,right_lib.mly)
	@bottom_export=$$(awk '/^%public / { print $$2; exit }' bottom_lib.mly); \
	bottom_export=$${bottom_export%:}; \
	if [ -z "$$bottom_export" ]; then \
	  echo "Expected %public export in bottom_lib.mly." >&2; \
	  exit 1; \
	fi; \
	rg -F -q -- "$$bottom_export" left_lib.mly || { echo "Missing $$bottom_export in left_lib.mly" >&2; exit 1; }; \
	rg -F -q -- "$$bottom_export" right_lib.mly || { echo "Missing $$bottom_export in right_lib.mly" >&2; exit 1; }
	$(call assert_files_absent,bottom_parse.mly left_parse.mly right_parse.mly main_parse.mly)
	$(call assert_no_internal_import_names,main_parser.mly)

$(eval $(call clean_rule,$(CLEAN_FILES)))
