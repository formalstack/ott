include ../common.mk

SCENARIOS := tex_inline tex_import_meta_hom_missing tex_pp_inconsistent_structure tex_transitive_collision tex_transitive_subst_shadow

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS):
	$(MAKE) -C $@

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
