include ../../common.mk

.PHONY: all clean

all:
	@rm -f Main.tex .out
	@$(OTT) -I . -tex_wrap false -i main_a.ott -i main_b.ott -o Main.tex > .out 2>&1
	$(call assert_contains,Main.tex,"\\ottkw{store\\_ok\\_q} \\$(COMMA) \\ottmv{q}")
	$(call assert_contains,Main.tex,"\\ottkw{store\\_ok\\_i} \\$(COMMA) \\ottmv{i}")
	$(call assert_contains,Main.tex,"\\ottkw{store\\_ok} \\$(COMMA) \\ottmv{q}}{%")
	$(call assert_contains,Main.tex,"\\ottkw{store\\_ok} \\$(COMMA) \\ottmv{i}}{%")
	$(call assert_absent,Main.tex,"\\ottkw{store\\_ok} \\$(COMMA) \\ottmv{index}}{%")
	$(call assert_absent,.out,"Fatal error: exception Not_found")
	$(call assert_absent,Main.tex,"<<no parses")

clean:
	rm -f Main.tex .out
