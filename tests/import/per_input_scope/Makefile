include ../common.mk

SCENARIOS := multiinput multiinput_same_module_backend_ids multiinput_same_file_different_aliases tex_multiinput_root_scope tex_multiinput_metavar_root_scope

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
