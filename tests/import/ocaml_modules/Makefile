include ../common.mk

OCAMLC := ocamlc

.PHONY: all clean ott compile consumer

all: consumer

ott: Lib_ast.ml Main.ml

Lib_ast.ml: lib.ott
	$(OTT) -i $< -o $@

Main.ml: main.ott lib.ott
	$(OTT) -i $< -o $@

Lib_ast.cmi Lib_ast.cmo: Lib_ast.ml
	$(OCAMLC) -c Lib_ast.ml

Main.cmi Main.cmo: Main.ml Lib_ast.cmi
	$(OCAMLC) -c Main.ml

check.cmo: check.ml Main.cmi Lib_ast.cmi
	$(OCAMLC) -c check.ml

compile: Lib_ast.cmo Main.cmo check.cmo

consumer: compile
	$(call assert_contains,Main.ml,"Wrapped_wrap of Lib_ast.term")
	$(call assert_contains,Main.ml,"let use_term (u : Lib_ast.term) : wrapped = Wrapped_wrap u")
	$(call assert_contains,Main.ml,"let keep_local_name () =")
	$(call assert_contains,Main.ml,"let term = 0 in")
	$(call assert_contains,Main.ml,"let label = \"term\" in")
	$(call assert_absent,Main.ml,"let Lib_ast.term = 0")
	$(call assert_absent,Main.ml,"\"Lib_ast.term\"")
	$(call assert_absent,Main.ml,"type term =")
	$(call assert_absent,Main.ml,"Term_var of")

clean:
	@rm -f Lib_ast.ml Main.ml *.cmi *.cmo *.cmx *.o
