BINS = unruly geewhiz shout
#BINS = unruly fall_of_rome geewhiz runpy

all: $(BINS)

%: %.lhs
	ghc -Wall --make $@

clean:
	-$(RM) *.{o,hi}
	-$(RM) *.html
	-$(RM) -- *.png
	-$(RM) $(BINS)

test: shout
	pandoc -F ./$< $<.lhs -o $<.html

.PHONY: all test clean
