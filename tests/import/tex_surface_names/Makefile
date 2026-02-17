include ../common.mk

SCENARIOS := tex_kwG_import tex_link_imported_synonym_root tex_synonym_rename_collision

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS):
	$(MAKE) -C $@

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
