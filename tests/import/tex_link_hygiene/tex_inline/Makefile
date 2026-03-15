include ../../common.mk

.PHONY: all clean

all: Main.tex Direct.tex Ambig.tex
	$(call assert_contains,Main.tex,"\\newcommand{\\ottq}")
	$(call assert_contains,Main.tex,"\\ottnt{q}")
	$(call assert_contains,Main.tex,"store\\_ok")
	$(call assert_contains,Main.tex,"helper\\_ok")
	$(call assert_contains,Main.tex,"gamma\\_ok")
	$(call assert_contains,Main.tex,"\\\\Gamma")
	$(call assert_absent,Main.tex,"skip\\_ok")
	$(call assert_absent,Main.tex,"%% import:")
	$(call assert_no_internal_tex_names,Main.tex)
	$(call assert_contains,Direct.tex,"show\\_ok")
	$(call assert_contains,Direct.tex,"\\\\Gamma")
	$(call assert_no_internal_tex_names,Direct.tex)
	$(call assert_contains,Ambig.tex,"\\\\Gamma")
	$(call assert_absent,Ambig.tex,"\\ottkw{G}")
	$(call assert_no_internal_tex_names,Ambig.tex)

Main.tex: main.ott lib.ott dep.ott
	$(OTT) -I . -tex_wrap false -o $@ $<

Direct.tex: direct.ott dep.ott
	$(OTT) -I . -tex_wrap false -o $@ $<

Ambig.tex: ambig_main.ott ambig_dep.ott
	$(OTT) -I . -tex_wrap false -o $@ $<

clean:
	rm -f Main.tex Direct.tex Ambig.tex *.v *.vo *.vos *.vok *.glob .*.aux .neg_*.out
