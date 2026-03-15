include ../common.mk

CLEAN_FILES := \
  $(call menhir_generated_files,rename_provider rename public public_provider transitive transitive_provider transitive_tokens) \
  $(call menhir_generated_objects,rename_provider rename public public_provider transitive transitive_provider transitive_tokens) \
  tokens.ml tokens.cmi tokens.cmo \
  check check.cmi check.cmo check.out

.PHONY: all clean rename_case public_case transitive_case

all: rename_case public_case transitive_case

$(eval $(call menhir_root_rule,rename_provider,))
$(eval $(call menhir_root_rule,rename,rename_provider.ott))
$(eval $(call menhir_root_rule,public_provider,,public_provider_lib.mly))
$(eval $(call menhir_root_rule,public,public_provider.ott))
$(eval $(call menhir_root_rule,transitive_tokens))
$(eval $(call menhir_root_rule,transitive_provider,transitive_tokens.ott,transitive_provider_lib.mly))
$(eval $(call menhir_root_rule,transitive,transitive_provider.ott transitive_tokens.ott))
$(eval $(call menhir_compile_root,rename_provider,,,))
$(eval $(call menhir_compile_root,rename,$(call menhir_ast_object,rename_provider),,))
$(eval $(call menhir_compile_root,public_provider,,,))
$(eval $(call menhir_compile_root,public,$(call menhir_ast_object,public_provider),$(call menhir_lib_source,public_provider),))
$(eval $(call menhir_compile_root,transitive_tokens,,,))
$(eval $(call menhir_compile_root,transitive_provider,tokens.cmo,,))
$(eval $(call menhir_compile_root,transitive,tokens.cmo $(call menhir_ast_object,transitive_provider),$(call menhir_lib_source,transitive_provider),))

tokens.ml: $(call menhir_root_output,transitive_tokens)
	cp transitive_tokens_ast.ml tokens.ml

tokens.cmo: tokens.ml
	$(OCAMLC) -c tokens.ml

check.cmo: check.ml $(call menhir_parser_object,rename) $(call menhir_lexer_object,rename) $(call menhir_ast_object,rename)
	$(OCAMLC) -c check.ml

check: check.cmo $(call menhir_ast_object,rename_provider) $(call menhir_ast_object,rename) $(call menhir_parser_object,rename) $(call menhir_lexer_object,rename)
	$(OCAMLC) -o check rename_provider_ast.cmo rename_ast.cmo rename_parser.cmo rename_lexer.cmo check.cmo

rename_case: check
	./check > check.out
	$(call assert_contains,rename_parser.mly,"%token <string> FOO")
	$(call assert_contains,rename_parser.mly,"%left FOO")
	$(call assert_absent,rename_parser.mly,"%left IDENT")
	$(call assert_contains,rename_parser.mly,"%prec FOO")
	$(call assert_files_absent,rename_parse.mly)
	$(call assert_contains,rename_lexer.mll,"['a'-'z']+ as foo")
	$(call assert_contains,check.out,"ok")

public_case: $(call menhir_parser_object,public) $(call menhir_lexer_object,public)
	$(call assert_absent,public_parser.mly,"%left IDENT")
	$(call assert_absent,public_provider_lib.mly,"%left IDENT")
	$(call assert_files_absent,public_parse.mly public_provider_parse.mly)
	$(call assert_contains,public_lexer.mll,"['a'-'z']+ as")

transitive_case: $(call menhir_parser_object,transitive) $(call menhir_lexer_object,transitive)
	$(call assert_absent,transitive_parser.mly,"%left IDENT")
	$(call assert_absent,transitive_provider_lib.mly,"%left IDENT")
	$(call assert_files_absent,transitive_parse.mly transitive_provider_parse.mly)
	$(call assert_contains,transitive_lexer.mll,"['a'-'z']+ as")

$(eval $(call clean_rule,$(CLEAN_FILES)))
