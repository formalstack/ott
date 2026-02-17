include ../../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -o /tmp/Neg.v transitive_scope_bad.ott,.neg_transitive_scope_bad.out,/tmp/Neg.v)
	$(call assert_contains,.neg_transitive_scope_bad.out,"File transitive_scope_bad.ott")
	$(call assert_contains,.neg_transitive_scope_bad.out,"Illegal reference to imported name 'ty'.")
	$(call assert_contains,.neg_transitive_scope_bad.out,"This name is only available transitively via imports.")
	$(call assert_absent,.neg_transitive_scope_bad.out,"no parses of")

clean:
	rm -f .neg_*.out
