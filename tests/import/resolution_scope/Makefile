include ../common.mk

SCENARIOS := \
  basic \
  direct \
  collision \
  diamond \
  negative_transitive_scope \
  negative_nested_scope_validation \
  negative_judg_transitive

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS):
	$(MAKE) -C $@

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
