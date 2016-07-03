TYPESCRIPT = tsc
TSFLAGS = --target ES5

FILES = $(addsuffix .js,offset lfsr)

all: $(FILES)

test: $(FILES)
	node test.js

%.js: %.ts
	$(TYPESCRIPT) $(TSFLAGS) $<
