include ../common.mk

SCENARIOS := rename_synonym rename_existing_synonym synonym_ambig

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS):
	$(MAKE) -C $@

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
