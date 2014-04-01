#
# Makefile for CS 11 Haskell track, lab 4.
#

GHC    = ghc --make
HFLAGS = -W -O
PROGS  = hello1 hello2 hello3 hcat hsort huniq

compile: $(PROGS)

% : %.hs
	$(GHC) $(HFLAGS) $< $(LIBS) -o $@

check:
	haskell_style_check *.hs

clean:
	rm -f *.hi *.o $(PROGS)
