include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main conflict) \
  $(call menhir_generated_objects,lib main conflict) \
  check check.cmi check.cmo check.out conflict.out

.PHONY: all clean comment_policy conflict_rejected

all: comment_policy conflict_rejected

$(eval $(call menhir_root_rule,lib,,lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call ast_objects_rule,lib,))
$(eval $(call ast_objects_rule,main,$(call menhir_ast_object,lib)))
$(eval $(call parser_objects_rule,lib,$(call menhir_ast_object,lib),))
$(eval $(call parser_objects_rule,main,$(call menhir_ast_object,lib) $(call menhir_ast_object,main),$(call menhir_lib_source,lib)))
$(eval $(call lexer_objects_rule,lib,$(call menhir_parser_object,lib)))
$(eval $(call lexer_objects_rule,main,$(call menhir_parser_object,main)))

check.cmo: check.ml $(call menhir_parser_object,lib) $(call menhir_lexer_object,lib) $(call menhir_parser_object,main) $(call menhir_lexer_object,main) $(call menhir_ast_object,main) $(call menhir_ast_object,lib)
	$(OCAMLC) -c check.ml

check: check.cmo $(call menhir_ast_object,lib) $(call menhir_ast_object,main) $(call menhir_parser_object,lib) $(call menhir_lexer_object,lib) $(call menhir_parser_object,main) $(call menhir_lexer_object,main)
	$(OCAMLC) -o check lib_ast.cmo main_ast.cmo lib_parser.cmo lib_lexer.cmo main_parser.cmo main_lexer.cmo check.cmo

comment_policy: check
	./check > check.out
	$(call assert_contains,lib_lexer.mll,"| \"//\"")
	$(call assert_contains,main_lexer.mll,"| \"#\"")
	$(call assert_absent,main_lexer.mll,"| \"//\"")
	$(call assert_contains,main_lexer.mll,"['a'-'z']+ as ident")
	$(call assert_contains,check.out,"ok")

conflict_rejected:
	$(call assert_command_fails,$(OTT) -i conflict.ott -o conflict_ast.ml -o conflict_parser.mly -o conflict_lexer.mll,conflict.out,)
	$(call assert_contains,conflict.out,"conflicting local lex-comment markers")

$(eval $(call clean_rule,$(CLEAN_FILES)))
