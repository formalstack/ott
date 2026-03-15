include ../../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -o /tmp/Bad.v main.ott,.duplicate_import_hom.out,/tmp/Bad.v)
	$(call assert_contains,.duplicate_import_hom.out,"File main.ott")
	$(call assert_contains,.duplicate_import_hom.out,"duplicate import hom \"rocq\"")

clean:
	rm -f .duplicate_import_hom.out
