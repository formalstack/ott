include ../../common.mk

.PHONY: all clean

all: Main.tex
	$(call assert_contains,Main.tex,"use\\_a\\_ok")
	$(call assert_contains,Main.tex,"use\\_b\\_ok")
	$(call assert_contains,Main.tex,"\\\\Gamma")
	$(call assert_absent,Main.tex,"%% import:")
	$(call assert_no_internal_tex_names,Main.tex)

Main.tex: main.ott lib_a.ott lib_b.ott dep.ott
	$(OTT) -I . -tex_wrap false -o $@ $<

clean:
	rm -f Main.tex *.v *.vo *.vos *.vok *.glob .*.aux .neg_*.out
