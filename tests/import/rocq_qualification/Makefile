include ../common.mk

SCENARIOS := embedded_syntax_qual embedded_syntax_hom_qual hom_binder_not_qualified hidden_ctor_qual listtuple_proj

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS):
	$(MAKE) -C $@

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
