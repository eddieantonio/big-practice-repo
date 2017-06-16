BIN = binary
LIBS = $(addsuffix .dylib, binary_add binary_mul)

CFLAGS = -std=c11 -Wall -Werror
LDLIBS = -ldl

all: $(BIN) $(LIBS)
$(BIN): $(BIN).o

%.dylib: %.o
	libtool -dynamic -compatibility_version 1 -current_version 1 -macosx_version_min 10.11 -o $@ $<

clean:
	$(RM) $(BIN) $(wildcard *.o) $(wildcard *.dylib) $(wildcard *.so)

test: $(BIN) $(LIBS)
	chmod a+rx binary_mul.dylib
	./$< add 13 7
	./$< mul 13 7
	! ./$< exp 13 7
	! ./$< add herp 7
	! ./$< mul 13 derp
	! ./$< add 13
	! ./$< add
	! ./$< add 13 7 42
	chmod a-rx binary_mul.dylib
	! ./$< mul 13 7
