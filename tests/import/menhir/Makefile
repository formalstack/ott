SUITES := \
  artifact_contract \
  binder_uniqueness \
  composition \
  declaration_policy \
  lexer_policy \
  ocaml_path_only \
  provider_scope_alias \
  provider_namespace_identity \
  public_visibility \
  namespace_stability \
  provider_ocaml_sugar \
  token_surface \
  transitive_libs

.PHONY: all clean $(SUITES)

all: $(SUITES)

$(SUITES): $(OTT_BIN)
	$(MAKE) -C $@ all

clean:
	@for d in $(SUITES); do \
	  $(MAKE) -C $$d clean; \
	done
