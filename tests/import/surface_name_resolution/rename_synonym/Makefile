include ../../common.mk

.PHONY: all clean ott rocq consumer default_avoid

all: rocq consumer default_avoid

ott: Lib.v Main.v

Lib.v: lib.ott
	$(OTT) -o $@ $<

Main.v: main.ott lib.ott
	$(OTT) -o $@ $<

Lib.vo: Lib.v
	$(ROCQ) $<

Main.vo: Main.v Lib.vo
	$(ROCQ) $<

check.vo: check.v Main.vo Lib.vo
	$(ROCQ) $<

rocq: Lib.vo Main.vo

consumer: check.vo
	$(call assert_contains,Main.v,"value_list:list Lib.value")
	$(call assert_contains,Main.v,"Lib.Store value_list")

default_avoid:
	@$(REPO_ROOT)/bin/ott -quiet -signal_parse_errors true -o /tmp/rename_synonym_default_avoid.v main.ott > .default_avoid.out 2>&1
	$(call assert_no_internal_import_names,.default_avoid.out)

clean:
	$(call clean_rocq_outputs,check.v,.default_avoid.out)
