include ../common.mk

SCENARIOS := \
  defnlang_import_keyword \
  import_menhir_hom_rejected \
  import_hom_validation \
  import_path_header

.PHONY: all clean $(SCENARIOS)

all: $(SCENARIOS)

$(SCENARIOS): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SCENARIOS); do \
	  $(MAKE) -C $$d clean; \
	done
