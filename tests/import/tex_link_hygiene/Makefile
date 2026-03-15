include ../common.mk

SCENARIOS := tex_inline tex_inline_same_file_different_aliases tex_import_meta_hom_missing tex_pp_inconsistent_structure tex_transitive_collision tex_transitive_subst_shadow

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
