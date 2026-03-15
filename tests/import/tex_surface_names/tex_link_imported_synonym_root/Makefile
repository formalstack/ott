include ../../common.mk

.PHONY: all clean

all: Main.tex
	$(call assert_contains,Main.tex,"\\ottmv{x}")
	$(call assert_absent_re,Main.tex,"__ott_trans_[0-9a-f]{8}_x")
	$(call assert_no_internal_tex_names,Main.tex)

Main.tex: main.ott dep.ott
	@$(OTT) -I . -tex_wrap false -o $@ $< > .out 2>&1

clean:
	rm -f Main.tex .out .neg_*.out
