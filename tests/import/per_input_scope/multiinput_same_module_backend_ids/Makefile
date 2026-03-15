include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: LibA.v LibB.v MultiInput.v MultiInputMerged.v

LibA.v: lib.ott
	$(OTT) -o $@ $<

LibB.v: lib.ott
	$(OTT) -o $@ $<

MultiInput.v: main_a.ott main_b.ott lib.ott
	@$(OTT) -i main_a.ott -i main_b.ott -o MultiInput.v > .out 2>&1

MultiInputMerged.v: main_a.ott main_b.ott lib.ott
	@$(OTT) -preserve-structure false -i main_a.ott -i main_b.ott -o MultiInputMerged.v > .merged.out 2>&1

LibA.vo: LibA.v
	$(ROCQ) $<

LibB.vo: LibB.v
	$(ROCQ) $<

MultiInput.vo: MultiInput.v LibA.vo LibB.vo
	$(ROCQ) $<

MultiInputMerged.vo: MultiInputMerged.v LibA.vo LibB.vo
	$(ROCQ) $<

check.vo: check.v MultiInput.vo LibA.vo LibB.vo
	$(ROCQ) $<

check_merged.vo: check_merged.v MultiInputMerged.vo LibA.vo LibB.vo
	$(ROCQ) $<

rocq: LibA.vo LibB.vo MultiInput.vo MultiInputMerged.vo

consumer: check.vo check_merged.vo
	$(call assert_contains,MultiInput.v,"Require LibA.")
	$(call assert_contains,MultiInput.v,"Require LibB.")
	$(call assert_contains_typed_binder,MultiInput.v,ATm_imported_a,LibA\.tm,\.)
	$(call assert_contains_typed_binder,MultiInput.v,BTm_imported_b,LibB\.tm,\.)
	$(call assert_contains,MultiInputMerged.v,"Require LibA.")
	$(call assert_contains,MultiInputMerged.v,"Require LibB.")
	$(call assert_contains_typed_binder,MultiInputMerged.v,ATm_imported_a,LibA\.tm,\.)
	$(call assert_contains_typed_binder,MultiInputMerged.v,BTm_imported_b,LibB\.tm,\.)
	$(call assert_absent,.out,"Conflicting module identifiers")
	$(call assert_absent,.merged.out,"Conflicting module identifiers")

clean:
	$(call clean_rocq_outputs,check.v check_merged.v,.out .merged.out)
