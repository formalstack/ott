include ../../common.mk

.PHONY: all clean

all: Main.tex
	$(call assert_contains,Main.tex,"ctx_ok")
	$(call assert_no_internal_tex_names,Main.tex)

Main.tex: main.ott lib.ott other.ott
	$(OTT) -I . -tex_wrap false -o $@ $<

clean:
	rm -f Main.tex *.v *.vo *.vos *.vok *.glob .*.aux .neg_*.out
