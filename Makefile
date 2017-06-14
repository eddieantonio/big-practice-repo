binary: binary.o binary_add.dylib

%.dylib: %.c
	$(CC) -dynamiclib $(CFLAGS) $(CPPFLAGS) -o $@ $<

test: binary
	./$< 13 7
