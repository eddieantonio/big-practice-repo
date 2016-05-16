TYPESCRIPT = tsc
TSFLAGS = --target ES5

all: lfsr.js

test: lfsr.js
	node test.js

%.js: %.ts
	$(TYPESCRIPT) $(TSFLAGS) $<
