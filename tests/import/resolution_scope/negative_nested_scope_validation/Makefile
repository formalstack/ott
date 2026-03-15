include ../../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -I . -o /tmp/NegNested.v transitive_main.ott,.neg_transitive.out,/tmp/NegNested.v)
	$(call assert_contains,.neg_transitive.out,"dep_transitive_bad.ott")
	$(call assert_contains,.neg_transitive.out,"Illegal reference to imported name 'cast'.")
	$(call assert_contains,.neg_transitive.out,"This name is only available transitively via imports.")
	$(call assert_absent,.neg_transitive.out,"no parses of")
	$(call assert_command_fails,$(OTT) -I . -o /tmp/NegNested.v collision_main.ott,.neg_collision.out,/tmp/NegNested.v)
	$(call assert_contains,.neg_collision.out,"dep_collision_bad.ott")
	$(call assert_contains,.neg_collision.out,"Imported name 'tm' from module 'base' conflicts with a local declaration.")
	$(call assert_contains,.neg_collision.out,"Use a rename in the import block to avoid the collision.")
	$(call assert_absent,.neg_collision.out,"no parses of")

clean:
	rm -f .neg_*.out
