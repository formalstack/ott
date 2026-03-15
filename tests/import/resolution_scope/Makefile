include ../common.mk

SCENARIOS := \
  basic \
  direct \
  path_search_nested \
  collision \
  diamond \
  diamond_same_file_different_aliases \
  missing_path \
  negative_transitive_scope \
  negative_nested_scope_validation \
  negative_judg_transitive

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
