#
# Makefile for CS 11 Haskell track, lab 5.
#

GHC    = ghc --make
HFLAGS = -W -O
LIBS   = 
#LIBS   = -lgmp -L/usr/lib
PROG   = triangle_game

compile: $(PROG)

% : %.hs
	$(GHC) $(HFLAGS) $< $(LIBS) -o $@

test:
	$(PROG) > $(PROG).output
	more $(PROG).output

clean:
	rm -f *.hi *.o $(PROG) $(PROG).output

check:
	haskell_style_check $(PROG).hs
