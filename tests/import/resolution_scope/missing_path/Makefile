include ../../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -I vendor -o /tmp/Missing.v main.ott,.neg_missing.out,/tmp/Missing.v)
	$(call assert_contains,.neg_missing.out,"Cannot find imported module 'ghost' at path 'nested/missing.ott'.")
	$(call assert_contains,.neg_missing.out,"./nested/missing.ott")
	$(call assert_contains,.neg_missing.out,"vendor/nested/missing.ott")
	$(call assert_absent,.neg_missing.out,"Problem parsing:")

clean:
	rm -f .neg_missing.out
