# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif
OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

PROJDIR := ..
BUILDDIR := .

# Create the list of directories
DIRS = Main Lexer Parser
SOURCEDIRS = $(foreach dir, $(DIRS), $(addprefix $(PROJDIR)/, $(dir)))
TARGETDIRS = $(foreach dir, $(DIRS), $(addprefix $(BUILDDIR)/, $(dir)))

# Create a list of *.ml sources in DIRS
SOURCESML = $(foreach dir, $(SOURCEDIRS), $(wildcard $(dir)/*.ml))

# Create a list of *.mli sources in DIRS
SOURCESMLI = $(foreach dir, $(SOURCEDIRS), $(wildcard $(dir)/*.mli))

# Define objects for all sources
OBJSCMO = $(subst $(PROJDIR), $(BUILDDIR), $(SOURCESML:.ml=.cmo))
OBJSCMI = $(subst $(PROJDIR), $(BUILDDIR), $(SOURCESML:.mli=.cmi))

# Generate the OCAMLC, OCAMLDEP includes parameters by adding -I before each source folder
COMPINCLUDES = $(foreach dir, $(TARGETDIRS), $(addprefix -I , $(dir)))
DEPINCLUDES = $(foreach dir, $(SOURCEDIRS), $(addprefix -I , $(dir)))

# Add this list to VPATH, the place make will look for the source files
VPATH = $(SOURCEDIRS)

# Define the function that will generate each rule
define generateRules
$(1)/%.cmi: %.mli
	@echo Building $$@
	$(OCAMLC) $$(COMPINCLUDES) -o ./$$@ -c $$<

$(1)/%.cmo: %.ml $(1)/%.cmi
	@echo Building $$@
	$(OCAMLC) $$(COMPINCLUDES) -o ./$$@ -c $$<
endef

all: directories depend pcl$(EXE)

depend: Lexer.ml Parser.ml $(SOURCESML) $(SOURCESMLI)
	$(OCAMLDEP) $(DEPINCLUDES) $^ | perl -pe 's|\Q$(PROJDIR)\E|$(BUILDDIR)|g' > $(BUILDDIR)/.depend

Lexer.ml: Lexer.mll
	ocamllex -o ../Lexer/$@ $<

Parser.ml: Parser.mly
	menhir -v $<

# Generate rules
$(foreach targetdir, $(TARGETDIRS), $(eval $(call generateRules, $(targetdir))))

-include .depend

directories:
	mkdir -p $(TARGETDIRS)

pcl$(EXE): $(OBJSCMO)
	@echo $(OBJSCMO)
	$(OCAMLC) $(OCAMLC_FLAGS) $(COMPINCLUDES) -o $@ $^

.PHONY: clean distclean

clean:
	$(RM) -r $(TARGETDIRS) $(BUILDDIR)/.depend $(PROJDIR)/Lexer/Lexer.ml $(PROJDIR)/Parser/Parser.ml \
	$(PROJDIR)/Parser/Parser.mli $(PROJDIR)/Parser/Parser.automaton $(PROJDIR)/Parser/Parser.conflicts

distclean: clean
	$(RM) $(BUILDDIR)/pcl
