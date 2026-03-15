include ../../common.mk

.PHONY: all clean ott rocq consumer

all: rocq consumer

ott: Collision_lib.v Collision.v

Collision_lib.v: collision_lib.ott
	$(OTT) -o $@ $<

Collision.v: collision.ott collision_lib.ott
	$(OTT) -o $@ $<

Collision_lib.vo: Collision_lib.v
	$(ROCQ) $<

Collision.vo: Collision.v Collision_lib.vo
	$(ROCQ) $<

check.vo: check.v Collision.vo Collision_lib.vo
	$(ROCQ) $<

rocq: Collision_lib.vo Collision.vo

consumer: check.vo
	$(call assert_contains,Collision.v,"Require Collision_lib.")
	$(call assert_contains_typed_binder,Collision.v,Expr_cont,Collision_lib\.container,)
	$(call assert_contains_ident,Collision.v,Collision_lib\.wrap_ok ,)

clean:
	$(call clean_rocq_outputs,check.v)
