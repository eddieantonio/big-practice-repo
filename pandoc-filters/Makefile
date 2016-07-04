BINS = unruly geewhiz shout runpy romulus

all: $(BINS)

%: %.lhs
	ghc -Wall --make $@

clean:
	-$(RM) *.{o,hi}
	-$(RM) *.html
	-$(RM) -- *.png
	-$(RM) $(BINS)

test: romulus
	pandoc -F ./$< $<.lhs -o $<.html

.PHONY: all test clean
