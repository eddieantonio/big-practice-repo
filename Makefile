CFLAGS = -Wall -Werror

rev:

%.llvm: %.c
	clang -S -c -O2 -emit-llvm $< -o $@

.PHONY: test
test: rev
	./rev
