include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: Base.v Dep_a.v Dep_b.v Diamond.v

Base.v: base.ott
	$(OTT) -o $@ $<

Dep_a.v: dep_a.ott base.ott
	$(OTT) -o $@ $<

Dep_b.v: dep_b.ott base.ott
	$(OTT) -o $@ $<

Diamond.v: diamond.ott dep_a.ott dep_b.ott base.ott
	$(OTT) -o $@ $<

Base.vo: Base.v
	$(ROCQ) $<

Dep_a.vo: Dep_a.v Base.vo
	$(ROCQ) $<

Dep_b.vo: Dep_b.v Base.vo
	$(ROCQ) $<

Diamond.vo: Diamond.v Dep_a.vo Dep_b.vo
	$(ROCQ) $<

check.vo: check.v Diamond.vo Dep_a.vo Dep_b.vo
	$(ROCQ) $<

rocq: Base.vo Dep_a.vo Dep_b.vo Diamond.vo

consumer: check.vo
	$(call assert_contains,Dep_a.v,"Require Base.")
	$(call assert_contains,Dep_b.v,"Require Base.")
	$(call assert_contains,Diamond.v,"Require Dep_a.")
	$(call assert_contains,Diamond.v,"Require Dep_b.")
	$(call assert_contains_typed_binder,Diamond.v,Expr_term,Dep_a\.tm,)
	$(call assert_contains_re,Diamond.v,"Expr_annot $(call typed_binder_re,Dep_b\.pty) $(call typed_binder_re,expr)")

clean:
	$(call clean_rocq_outputs,check.v)
