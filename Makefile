# simple makefile for
#

SOURCEDIR=src
BUILDDIR=build
LANG=C

all: $(SOURCEDIR)
	ghc --make $(SOURCEDIR)/Main.hs -i$(SOURCEDIR) -outputdir $(BUILDDIR) -o $(BUILDDIR)/Main
	

clean:
	rm -rf build/*
