BIN = binary
LIBS = $(addsuffix .dylib, binary_add binary_mul)

all: $(BIN) $(LIBS)
$(BIN): $(BIN).o

%.dylib: %.o
	libtool -dynamic -compatibility_version 1 -current_version 1 -macosx_version_min 10.11 -o $@ $<

clean:
	$(RM) $(BIN) $(wildcard *.o) $(wildcard *.dylib) $(wildcard *.so)

test: $(BIN) $(LIBS)
	./$< add 13 7
	./$< mul 13 7
