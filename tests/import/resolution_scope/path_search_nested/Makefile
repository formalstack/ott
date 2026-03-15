include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: Lib.v Main.v

Lib.v: vendor/nested/lib.ott
	$(OTT) -o $@ $<

Main.v: main.ott vendor/nested/lib.ott
	$(OTT) -I vendor -o $@ $<

Lib.vo: Lib.v
	$(ROCQ) $<

Main.vo: Main.v Lib.vo
	$(ROCQ) $<

check.vo: check.v Main.vo Lib.vo
	$(ROCQ) $<

rocq: Lib.vo Main.vo

consumer: check.vo
	$(call assert_contains,Main.v,"Require Lib.")
	$(call assert_contains_typed_binder,Main.v,Expr_imported_val,Lib\.value,)

clean:
	$(call clean_rocq_outputs,check.v)
