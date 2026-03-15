include ../../common.mk

.PHONY: all clean

all:
	@rm -f Main.tex .out
	@$(OTT) -tex_wrap false -o Main.tex main.ott > .out 2>&1
	$(call assert_no_parse_failures,.out)
	$(call assert_contains,Main.tex,"\\ottkw{import}")
	$(call assert_contains,Main.tex,"\\ottkw{imported}")

clean:
	rm -f Main.tex .out
