include ../../common.mk

ROCQFLAGS := -Q . Foo.Bar -quiet -w "-all"
ROCQ := $(ROCQC) $(ROCQFLAGS)

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: Dep.v Main.v

Dep.v: dep.ott
	$(OTT) -o $@ $<

Main.v: main.ott dep.ott
	$(OTT) -o $@ $<

Dep.vo: Dep.v
	$(ROCQ) $<

Main.vo: Main.v Dep.vo
	$(ROCQ) $<

check.vo: check.v Main.vo Dep.vo
	$(ROCQ) $<

rocq: Dep.vo Main.vo

consumer: check.vo
	$(call assert_contains,Main.v,"From Foo.Bar Require Dep.")
	$(call assert_contains_typed_binder,Main.v,Expr_imported_ty,Dep\.ty,)

clean:
	$(call clean_rocq_outputs,check.v)
