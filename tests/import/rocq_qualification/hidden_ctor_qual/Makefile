include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

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
	$(call assert_contains,Main.v,"Inductive UUse : Lib.pointer -> Prop :=")
	$(call assert_contains,Main.v,"Lib.JPack pointer (Lib.val_ptr pointer)")

clean:
	$(call clean_rocq_outputs,check.v)
