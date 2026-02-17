include ../../common.mk

.PHONY: all clean

all:
	@rm -f MultiInput.v .out
	@$(OTT) -o MultiInput.v -showraw true -i a/main_a.ott -i b/main_b.ott > .out 2>&1
	$(call assert_contains,MultiInput.v,"from: no input files")
	$(call assert_contains,MultiInput.v,"Require LibA.")
	$(call assert_contains,MultiInput.v,"Require LibB.")
	$(call assert_absent,.out,"Fatal error")
	$(call assert_absent,.out,"Error:")

clean:
	rm -f MultiInput.v .out
