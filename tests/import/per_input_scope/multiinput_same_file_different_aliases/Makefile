include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: Base.v Common.v MultiInput.v MultiInputMerged.v

Base.v: lib.ott
	$(OTT) -o $@ $<

Common.v: lib.ott
	$(OTT) -o $@ $<

MultiInput.v: main_a.ott main_b.ott lib.ott
	@$(OTT) -i main_a.ott -i main_b.ott -o MultiInput.v > .out 2>&1

MultiInputMerged.v: main_a.ott main_b.ott lib.ott
	@$(OTT) -preserve-structure false -i main_a.ott -i main_b.ott -o MultiInputMerged.v > .merged.out 2>&1

Base.vo: Base.v
	$(ROCQ) $<

Common.vo: Common.v
	$(ROCQ) $<

MultiInput.vo: MultiInput.v Base.vo Common.vo
	$(ROCQ) $<

MultiInputMerged.vo: MultiInputMerged.v Base.vo Common.vo
	$(ROCQ) $<

check.vo: check.v MultiInput.vo Base.vo Common.vo
	$(ROCQ) $<

check_merged.vo: check_merged.v MultiInputMerged.vo Base.vo Common.vo
	$(ROCQ) $<

rocq: Base.vo Common.vo MultiInput.vo MultiInputMerged.vo

consumer: check.vo check_merged.vo
	$(call assert_contains,MultiInput.v,"Require Base.")
	$(call assert_contains,MultiInput.v,"Require Common.")
	$(call assert_contains_typed_binder,MultiInput.v,ATm_imported_a,Base\.tm,\.)
	$(call assert_contains_typed_binder,MultiInput.v,BTm_imported_b,Common\.tm,\.)
	$(call assert_contains,MultiInputMerged.v,"Require Base.")
	$(call assert_contains,MultiInputMerged.v,"Require Common.")
	$(call assert_contains_typed_binder,MultiInputMerged.v,ATm_imported_a,Base\.tm,\.)
	$(call assert_contains_typed_binder,MultiInputMerged.v,BTm_imported_b,Common\.tm,\.)
	$(call assert_absent,.out,"was already loaded as module")
	$(call assert_absent,.merged.out,"was already loaded as module")

clean:
	$(call clean_rocq_outputs,check.v check_merged.v,.out .merged.out)
