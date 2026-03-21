include ../common.mk

OTT_PARSING := $(OTT) -picky_multiple_parses true

.PHONY: all clean resolved unresolved unrelated

all: resolved unresolved unrelated

resolved: Terms.tex Formula.tex
	$(call assert_exists,Terms.tex)
	$(call assert_exists,Formula.tex)
	$(call assert_no_internal_tex_names,Terms.tex)
	$(call assert_no_internal_tex_names,Formula.tex)

unresolved:
	$(call assert_command_fails,$(OTT_PARSING) -tex_wrap false -o TermsUnresolved.tex terms_unresolved.ott,.terms_unresolved.out,TermsUnresolved.tex)
	$(call assert_contains,.terms_unresolved.out,"multiple parses of \"use_expr box unit end\":")
	$(call assert_contains,.terms_unresolved.out,"multiple parses of \"use_fundefn fold unit end\":")
	$(call assert_command_fails,$(OTT_PARSING) -tex_wrap false -o FormulaUnresolved.tex formula_unresolved.ott,.formula_unresolved.out,FormulaUnresolved.tex)
	$(call assert_contains,.formula_unresolved.out,"multiple parses of \"load unit ==> unit\":")

unrelated:
	$(call assert_command_fails,$(OTT_PARSING) -tex_wrap false -o Unrelated.tex unrelated.ott,.unrelated.out,Unrelated.tex)
	$(call assert_contains,.unrelated.out,"multiple parses of \"use x y\":")

Terms.tex: terms.ott lib.ott
	$(OTT_PARSING) -tex_wrap false -o $@ $<

Formula.tex: formula.ott lib.ott
	$(OTT_PARSING) -tex_wrap false -o $@ $<

clean:
	rm -f Terms.tex Formula.tex TermsUnresolved.tex FormulaUnresolved.tex \
	  Unrelated.tex .terms_unresolved.out .formula_unresolved.out \
	  .unrelated.out
