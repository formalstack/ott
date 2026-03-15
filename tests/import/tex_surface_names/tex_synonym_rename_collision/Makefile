include ../../common.mk

.PHONY: all clean

all:
	@rm -f Main.tex .neg_tex_synonym_rename_collision.out
	@$(OTT) -I . -tex_wrap false -o Main.tex main.ott > .neg_tex_synonym_rename_collision.out 2>&1
	$(call assert_contains,Main.tex,"\\ottkw{store\\_ok} \\$(COMMA) \\ottsym{<-}  \\ottnt{atom}}{%")
	$(call assert_absent,Main.tex,"\\ottkw{store\\_ok} \\$(COMMA) \\ottsym{<-}  \\ottnt{v}}{%")
	$(call assert_absent,.neg_tex_synonym_rename_collision.out,"Imported name 'value' conflicts")
	$(call assert_no_internal_tex_names,Main.tex)

clean:
	rm -f Main.tex *.v *.vo *.vos *.vok *.glob .*.aux .neg_*.out
