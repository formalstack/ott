include ../../common.mk

.PHONY: all clean

all: Main.v
	$(call assert_no_parse_failures,.out)
	$(call assert_contains,Main.v,"Require Lib.")

Main.v: main.ott deps/lib.ott
	@rm -f Main.v .out
	@$(OTT) -o $@ $< > .out 2>&1

clean:
	$(call clean_rocq_outputs,)
	@rm -f .out
