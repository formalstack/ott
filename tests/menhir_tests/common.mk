OTTDIR ?= ../../..

OTT ?= $(OTTDIR)/bin/ott
MENHIR ?= menhir
MENHIRFLAGS ?= --infer --base $(ROOT)_parser
MENHIR_EXTRA_MLY ?=
OCAMLBUILD_PACKAGES ?=
MAIN ?= main
OTTFLAGS ?=

MENHIR_INPUTS_FROM_BUILD := \
  $(foreach src,$(MENHIR_EXTRA_MLY),../$(src))

QUOTIENTED_PDF_OTTFLAGS ?= -quotient_rules true
UNQUOTIENTED_PDF_OTTFLAGS ?= -quotient_rules false

OCAMLBUILD_PACKAGE_FLAGS := $(foreach pkg,$(OCAMLBUILD_PACKAGES),-package $(pkg))
OCAMLBUILD := ocamlbuild -no-links -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS) $(MENHIR_INPUTS_FROM_BUILD)" $(OCAMLBUILD_PACKAGE_FLAGS)

GENERATED_BY_OTT := \
  $(ROOT)_ast.ml \
  $(ROOT)_parser.mly \
  $(ROOT)_parser_pp.ml \
  $(ROOT)_lexer.mll \
  $(ROOT)_lib.mly \
  $(ROOT).tex

.PHONY: all clean pdf realclean

all:
	$(OTT) $(OTTFLAGS) -show_sort true -quotient_rules false -i $(ROOT).ott -o $(ROOT)_parser.mly -o $(ROOT)_lexer.mll -o $(ROOT)_ast.ml -o $(ROOT).tex
	$(OCAMLBUILD) $(MAIN).byte

pdf: $(ROOT)_quotiented.pdf $(ROOT)_unquotiented.pdf

$(ROOT)_quotiented.pdf: $(ROOT).ott Makefile $(OTTDIR)/tests/menhir_tests/common.mk
	$(OTT) $(QUOTIENTED_PDF_OTTFLAGS) -i $(ROOT).ott -o $(ROOT)_quotiented.tex
	pdflatex $(ROOT)_quotiented.tex

$(ROOT)_unquotiented.pdf: $(ROOT).ott Makefile $(OTTDIR)/tests/menhir_tests/common.mk
	$(OTT) $(UNQUOTIENTED_PDF_OTTFLAGS) -i $(ROOT).ott -o $(ROOT)_unquotiented.tex
	pdflatex $(ROOT)_unquotiented.tex

clean:
	rm -rf *~
	rm -rf _build
	rm -rf $(GENERATED_BY_OTT)
	rm -rf $(ROOT)_quotiented.tex $(ROOT)_unquotiented.tex
	rm -rf *.aux *.log
	rm -rf $(MAIN).native $(MAIN).byte
	$(OCAMLBUILD) -clean

realclean:
	$(MAKE) clean
	rm -rf *.pdf
