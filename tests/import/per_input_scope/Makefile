include ../common.mk

SCENARIOS := multiinput multiinput_same_module_backend_ids tex_multiinput_root_scope tex_multiinput_metavar_root_scope

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS):
	$(MAKE) -C $@

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
