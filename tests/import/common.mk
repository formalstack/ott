REPO_ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../..)
MAKEFLAGS += --silent --no-print-directory
.DEFAULT_GOAL := all
OTT_BIN := $(REPO_ROOT)/bin/ott
OTT := $(OTT_BIN) -quiet -signal_parse_errors true -rocq_avoid 0
ROCQC := rocq c
ROCQFLAGS := -R . "" -quiet -w "-all"
ROCQ := $(ROCQC) $(ROCQFLAGS)
COMMA := ,
IDENT_RE := [A-Za-z_][A-Za-z0-9_']*

OTT_SOURCES := \
  $(REPO_ROOT)/Makefile \
  $(REPO_ROOT)/src/Makefile \
  $(wildcard $(REPO_ROOT)/src/*.ml) \
  $(wildcard $(REPO_ROOT)/src/*.mli) \
  $(wildcard $(REPO_ROOT)/src/*.mll) \
  $(wildcard $(REPO_ROOT)/src/*.mly) \
  $(wildcard $(REPO_ROOT)/src/*.tex)

$(OTT_BIN): $(OTT_SOURCES)
	@$(MAKE) -C $(REPO_ROOT)

all: $(OTT_BIN)

define assert_contains
	@rg -F -q -- $(2) $(1) >/dev/null 2>&1; \
	rc=$$?; \
	if [ $$rc -eq 0 ]; then \
	  :; \
	elif [ $$rc -eq 1 ]; then \
	  echo "Expected text not found in $(1)." >&2; \
	  exit 1; \
	else \
	  echo "Search failed in $(1)." >&2; \
	  exit 1; \
	fi
endef

define assert_absent
	@rg -F -q -- $(2) $(1) >/dev/null 2>&1; \
	rc=$$?; \
	if [ $$rc -eq 1 ]; then \
	  :; \
	elif [ $$rc -eq 0 ]; then \
	  echo "Unexpected text found in $(1)." >&2; \
	  exit 1; \
	else \
	  echo "Search failed in $(1)." >&2; \
	  exit 1; \
	fi
endef

define assert_exists
	@if [ -f $(1) ]; then \
	  :; \
	else \
	  echo "Expected file not found: $(1)." >&2; \
	  exit 1; \
	fi
endef

define assert_absent_re
	@rg -q -e $(2) $(1) >/dev/null 2>&1; \
	rc=$$?; \
	if [ $$rc -eq 1 ]; then \
	  :; \
	elif [ $$rc -eq 0 ]; then \
	  echo "Unexpected regex match found in $(1)." >&2; \
	  exit 1; \
	else \
	  echo "Regex search failed in $(1)." >&2; \
	  exit 1; \
	fi
endef

define assert_contains_re
	@rg -q -e $(2) $(1) >/dev/null 2>&1; \
	rc=$$?; \
	if [ $$rc -eq 0 ]; then \
	  :; \
	elif [ $$rc -eq 1 ]; then \
	  echo "Expected regex match not found in $(1)." >&2; \
	  exit 1; \
	else \
	  echo "Regex search failed in $(1)." >&2; \
	  exit 1; \
	fi
endef

define typed_binder_re
\\($(IDENT_RE):$(1)\\)
endef

define assert_contains_typed_binder
	$(call assert_contains_re,$(1),"$(2) $(call typed_binder_re,$(3))$(4)")
endef

define assert_contains_ident
	$(call assert_contains_re,$(1),"$(2)$(IDENT_RE)$(3)")
endef

define assert_no_parse_failures
	$(call assert_absent,$(1),"Problem parsing:")
	$(call assert_absent,$(1),"syntax error")
	$(call assert_absent,$(1),"no parses of")
endef

define assert_no_internal_tex_names
	$(call assert_absent,$(1),"__ott_mod")
	$(call assert_absent,$(1),"\\_\\_ott\\_mod\\_")
	$(call assert_absent,$(1),"ottXXmod")
	$(call assert_absent,$(1),"__ott_trans")
	$(call assert_absent,$(1),"\\_\\_ott\\_trans\\_")
	$(call assert_absent,$(1),"ottXXtrans")
	$(call assert_absent,$(1),"otttransiv")
endef

define assert_no_internal_import_names
	$(call assert_absent,$(1),"__ott_mod")
	$(call assert_absent,$(1),"__ott_trans_")
	$(call assert_absent,$(1),"otttransiv")
endef

define assert_command_fails
	@rm -f $(2) $(3)
	@set +e; \
	$(1) > $(2) 2>&1; \
	rc=$$?; \
	if [ $$rc -eq 0 ]; then \
	  echo "Expected command to fail: $(1)" >&2; \
	  [ -f $(2) ] && cat $(2) >&2; \
	  exit 1; \
	fi
endef

define clean_rocq_outputs
	@for f in *.v; do \
	  [ "$$f" = "*.v" ] && continue; \
	  case " $(strip $(1)) " in \
	    *" $$f "*) ;; \
	    *) rm -f "$$f" ;; \
	  esac; \
	done
	@rm -f *.vo *.vos *.vok *.glob .*.aux $(2)
endef
