include ../../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -o /tmp/Neg.v judg_transitive_bad.ott,.neg_judg_transitive_bad.out,/tmp/Neg.v)
	$(call assert_contains,.neg_judg_transitive_bad.out,"File judg_transitive_bad.ott")
	$(call assert_contains,.neg_judg_transitive_bad.out,"Error: ")
	$(call assert_contains,.neg_judg_transitive_bad.out,"no parses of \"container ok\"")
	$(call assert_absent,.neg_judg_transitive_bad.out,"Illegal reference to judgement class")
	$(call assert_absent,.neg_judg_transitive_bad.out,"Illegal reference to imported name")

clean:
	rm -f .neg_*.out
