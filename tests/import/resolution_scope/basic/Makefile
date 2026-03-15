include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: Base.v

Base.v: base.ott
	$(OTT) -o $@ $<

Base.vo: Base.v
	$(ROCQ) $<

check.vo: check.v Base.vo
	$(ROCQ) $<

rocq: Base.vo

consumer: check.vo
	$(call assert_contains,Base.v,"Inductive ty : Set :=")
	$(call assert_contains,Base.v,"| Ty_arrow (ty1:ty) (ty2:ty).")

clean:
	$(call clean_rocq_outputs,check.v)
