include ../../import/common.mk

.PHONY: all clean verify

all: parser.mly lexer.mll ast.ml parser_pp.ml verify

parser.mly lexer.mll ast.ml parser_pp.ml: shared.ott extra.ott
	@$(OTT) -i shared.ott -i extra.ott -o parser.mly -o lexer.mll -o ast.ml > .out 2>&1

verify: parser.mly lexer.mll ast.ml parser_pp.ml
	$(call assert_exists,parser.mly)
	$(call assert_exists,lexer.mll)
	$(call assert_exists,ast.ml)
	$(call assert_exists,parser_pp.ml)
	$(call assert_no_parse_failures,.out)
	@count=$$(rg -c '^%token <string> IDENT$$' parser.mly); \
	if [ "$$count" -eq 1 ]; then \
	  :; \
	else \
	  echo "Expected exactly one IDENT token declaration, found $$count." >&2; \
	  exit 1; \
	fi

clean:
	rm -f parser.mly lexer.mll ast.ml parser_pp.ml .out
