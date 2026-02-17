include common.mk

SUITES := \
  parser_boundary \
  resolution_scope \
  surface_name_resolution \
  rocq_qualification \
  per_input_scope \
  tex_surface_names \
  tex_link_hygiene

.PHONY: all clean $(SUITES)

all: $(SUITES)

$(SUITES):
	$(MAKE) -C $@

clean:
	@for d in $(SUITES); do \
	  $(MAKE) -C $$d clean; \
	done
