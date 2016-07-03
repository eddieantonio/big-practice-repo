TYPESCRIPT = tsc
TSFLAGS = --target ES5

all: lfsr.js

test: offset.js
	node test.js

%.js: %.ts
	$(TYPESCRIPT) $(TSFLAGS) $<
