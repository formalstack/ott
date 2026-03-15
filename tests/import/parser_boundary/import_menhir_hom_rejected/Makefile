include ../../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -o /tmp/Bad.v main.ott,.unsupported_menhir_hom.out,/tmp/Bad.v)
	$(call assert_contains,.unsupported_menhir_hom.out,"import-block menhir is no longer supported")
	$(call assert_contains,.unsupported_menhir_hom.out,"Menhir imports always use the {{ ocaml ... }} path")

clean:
	rm -f .unsupported_menhir_hom.out
