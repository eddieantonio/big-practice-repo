all: unruly geewhiz
#all: unruly shout fall_of_rome geewhiz runpy

%: %.lhs
	ghc -Wall --make $@

clean:
	-$(RM) *.{o,hi}

test: geewhiz
	pandoc -t plain -F ./$< $<.lhs

.PHONY: all test clean
