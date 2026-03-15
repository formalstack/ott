include common.mk

SUITES := \
  parser_boundary \
  resolution_scope \
  surface_name_resolution \
  rocq_qualification \
  ocaml_modules \
  menhir \
  per_input_scope \
  tex_surface_names \
  tex_link_hygiene

.PHONY: all clean $(SUITES)

all: $(SUITES)

$(SUITES): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SUITES); do \
	  $(MAKE) -C $$d clean; \
	done
