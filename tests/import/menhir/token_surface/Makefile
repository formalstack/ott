include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,lib main) \
  $(call menhir_generated_objects,lib main) \
  check check.cmi check.cmo check.out

.PHONY: all clean token_surface

all: token_surface

$(eval $(call menhir_root_rule,lib,,lib_lib.mly))
$(eval $(call menhir_root_rule,main,lib.ott))
$(eval $(call menhir_compile_root,lib,,,))
$(eval $(call menhir_compile_root,main,$(call menhir_ast_object,lib),$(call menhir_lib_source,lib),))

check.cmo: check.ml $(call menhir_ast_object,lib) $(call menhir_ast_object,main) $(call menhir_parser_object,main) $(call menhir_lexer_object,main)
	$(OCAMLC) -c check.ml

check: check.cmo $(call menhir_ast_object,lib) $(call menhir_ast_object,main) $(call menhir_parser_object,main) $(call menhir_lexer_object,main)
	$(OCAMLC) -o check lib_ast.cmo main_ast.cmo main_parser.cmo main_lexer.cmo check.cmo

token_surface: check
	./check > check.out
	$(call assert_contains,main_parser.mly,"%token BANG")
	$(call assert_contains,main_lexer.mll,"['a'-'z']+ as ident")
	$(call assert_absent,main_lexer.mll,"| \"!\"")
	$(call assert_absent_re,lib_lib.mly,"^term:")
	$(call assert_absent_re,lib_lib.mly,"^other:")
	$(call assert_contains,check.out,"ok")

$(eval $(call clean_rule,$(CLEAN_FILES)))
