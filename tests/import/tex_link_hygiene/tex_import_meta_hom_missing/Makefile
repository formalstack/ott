include ../../common.mk

.PHONY: all clean

all: Main.tex
	$(call assert_absent,.out,"meta or sugar production must have a hom for each target")
	$(call assert_contains,Main.tex,"ottnt{t}")
	$(call assert_no_internal_tex_names,Main.tex)

Main.tex: main.ott dep.ott
	@$(OTT) -I . -tex_wrap false -o $@ $< > .out 2>&1

clean:
	rm -f Main.tex .out .neg_*.out
