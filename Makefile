BINS = unruly geewhiz shout runpy
#BINS = fall_of_rome

all: $(BINS)

%: %.lhs
	ghc -Wall --make $@

clean:
	-$(RM) *.{o,hi}
	-$(RM) *.html
	-$(RM) -- *.png
	-$(RM) $(BINS)

test: runpy
	pandoc -F ./$< $<.lhs -o $<.html

.PHONY: all test clean
