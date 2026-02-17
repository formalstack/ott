include ../../common.mk

.PHONY: all clean

all: Main.tex
	$(call assert_contains,Main.tex,"store\\_ok")
	$(call assert_absent,.neg_tex_transitive_collision.out,"Imported name 'place' conflicts")
	$(call assert_no_internal_tex_names,Main.tex)

Main.tex: main.ott dep.ott
	@$(OTT) -I . -tex_wrap false -o $@ $< > .neg_tex_transitive_collision.out 2>&1

clean:
	rm -f Main.tex *.v *.vo *.vos *.vok *.glob .*.aux .neg_*.out
