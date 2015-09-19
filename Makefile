all: unruly geewhiz
#all: unruly shout fall_of_rome geewhiz runpy

%: %.lhs
	ghc -Wall --make $@

clean:
	-$(RM) *.{o,hi}

test: geewhiz
	pandoc -F ./$< $<.lhs -o $<.html

.PHONY: all test clean
