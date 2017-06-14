binary: binary.o binary_add.dylib binary_mul.dylib

%.dylib: %.c
	$(CC) -dynamiclib $(CFLAGS) $(CPPFLAGS) -o $@ $<

test: binary
	./$< add 13 7
	./$< mul 13 7
