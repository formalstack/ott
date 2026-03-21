include ../../common.mk

OTT_PARSING := $(OTT) -picky_multiple_parses true

.PHONY: all clean resolved unresolved

all: unresolved resolved

unresolved:
	$(call assert_command_fails,$(OTT_PARSING) -tex_wrap false -o MainUnresolved.tex -i a/main_a.ott -i b/main_b_unresolved.ott,.unresolved.out,MainUnresolved.tex)
	$(call assert_contains,.unresolved.out,"multiple parses of \"use box unit end\":")

resolved:
	@rm -f Main.tex .resolved.out
	@$(OTT_PARSING) -tex_wrap false -o Main.tex -i a/main_a.ott -i b/main_b.ott > .resolved.out 2>&1
	$(call assert_exists,Main.tex)
	$(call assert_no_internal_tex_names,Main.tex)

clean:
	rm -f Main.tex MainUnresolved.tex .resolved.out .unresolved.out
