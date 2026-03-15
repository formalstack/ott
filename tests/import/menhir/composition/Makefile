include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main) \
  check check.cmi check.cmo check.out

.PHONY: all clean composition

all: composition

$(eval $(call menhir_root_rule,lib,,lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call ast_objects_rule,lib,))
$(eval $(call ast_objects_rule,main,$(call menhir_ast_object,lib)))
$(eval $(call pp_objects_rule,lib,$(call menhir_ast_object,lib)))
$(eval $(call pp_objects_rule,main,$(call menhir_ast_object,main) $(call menhir_ast_object,lib) $(call menhir_pp_object,lib)))
$(eval $(call parser_objects_rule,main,$(call menhir_ast_object,lib) $(call menhir_ast_object,main),$(call menhir_lib_source,lib)))
$(eval $(call lexer_objects_rule,main,$(call menhir_parser_object,main)))

check.cmo: check.ml $(call menhir_parser_object,main) $(call menhir_lexer_object,main) $(call menhir_pp_object,main) $(call menhir_pp_object,lib) $(call menhir_ast_object,main) $(call menhir_ast_object,lib)
	$(OCAMLFIND) -I . -c check.ml

check: check.cmo $(call menhir_ast_object,lib) $(call menhir_ast_object,main) $(call menhir_pp_object,lib) $(call menhir_pp_object,main) $(call menhir_parser_object,main) $(call menhir_lexer_object,main)
	$(OCAMLFIND) -I . -linkpkg -package pprint -o check lib_ast.cmo main_ast.cmo lib_parser_pp.cmo main_parser_pp.cmo main_parser.cmo main_lexer.cmo check.cmo

composition: check
	./check > check.out
	$(call assert_contains,lib_lib.mly,"%public")
	$(call assert_absent,lib_lib.mly,"%token")
	$(call assert_absent,lib_lib.mly,"%start")
	$(call assert_absent,lib_lib.mly,"%right EOF")
	$(call assert_absent_re,lib_lib.mly,"^term:")
	$(call assert_contains,main_parser.mly,"%start <Lib_ast.term> consumer_start")
	$(call assert_contains,main_parser.mly,"%right EOF")
	$(call assert_contains,main_parser.mly,"wrapped:")
	$(call assert_contains,main_parser.mly,"consumer:")
	$(call assert_absent,main_parser.mly,"term:")
	$(call assert_contains,main_parser.mly,"let x : Lib_ast.term = match wrapped with Wrapped_wrap u -> u in")
	$(call assert_files_absent,lib_parser.cmi lib_parser.cmo lib_lexer.cmi lib_lexer.cmo)
	$(call assert_files_absent,lib_parse.mly main_parse.mly)
	$(call assert_contains,main_ast.ml,"consumer = Lib_ast.term")
	$(call assert_contains,main_lexer.mll,"IDENT")
	$(call assert_contains,main_lexer.mll,"['a'-'z']+ as ident")
	$(call assert_contains,main_parser_pp.ml,"module Make (Imported : Imported_pp) = struct")
	$(call assert_contains,main_parser_pp.ml,"Imported.Lib_ast_pp.pp_term term")
	$(call assert_contains,check.out,"Term_var(\"x\")")
	$(call assert_contains,check.out,"Wrapped_wrap(Term_var(\"x\"))")
	$(call assert_contains,check.out,"x")
	$(call assert_no_internal_import_names,main_parser_pp.ml)

$(eval $(call clean_rule,$(CLEAN_FILES)))
