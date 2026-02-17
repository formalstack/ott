include ../../common.mk

.PHONY: all clean

all: Main.tex
	$(call assert_contains,Main.tex,"\\\\Gamma")
	$(call assert_absent,Main.tex,"\\\\ottkw{G}")
	$(call assert_no_internal_tex_names,Main.tex)

Main.tex: main.ott dep.ott
	$(OTT) -I . -tex_wrap false -o $@ $<

clean:
	rm -f Main.tex .out .neg_*.out
