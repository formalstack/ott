include ../common.mk

SCENARIOS := embedded_syntax_qual embedded_syntax_hom_qual hom_binder_not_qualified hidden_ctor_qual import_from listtuple_proj

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
