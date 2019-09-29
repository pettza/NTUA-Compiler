# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif
OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

all: depend Lexer.cmo Parser.cmo

-include .depend

depend: Lexer.ml Lexer.mli Parser.ml Parser.mli
	$(OCAMLDEP) $^ > .depend

Lexer.cmi: Lexer.mli
	@echo Building $@
	$(OCAMLC) -o $@ $<

Lexer.cmo: Lexer.ml Lexer.cmi
	@echo Building $@
	$(OCAMLC) -o $@ $<

Parser.cmi: Parser.mli
	@echo Building $@
	$(OCAMLC) -o $@ $<

Parser.cmo: Parser.ml Parser.cmi
	@echo Building $@
	$(OCAMLC) -o $@ $<

Lexer.ml: Lexer.mll
	ocamllex -o Lexer/$@ $<

Parser.ml Parser.mli: Parser.mly
	menhir -v $< --unused-precedence-levels --unused-tokens

.PHONY: clean distclean

clean:
	$(RM) *.cmo *.cmi Lexer.ml Parser.ml Parser.mli Parser.conflicts Parser.automaton .depend

distclean: clean
