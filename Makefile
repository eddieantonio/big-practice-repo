BIN = binary
LIBS = $(addsuffix .$(SO), binary_add binary_mul)

CFLAGS = -std=c11 -Wall -Werror
LDLIBS = -ldl

ifeq ($(shell uname -s),Darwin)
SO = dylib
else
SO = so
LDEXTRA = -Wl,-rpath=$(PWD)
endif

all: $(BIN) $(LIBS)

$(BIN): LDFLAGS := $(LDFLAGS) $(LDEXTRA)
$(BIN): $(BIN).o

%.dylib: %.o
	libtool -dynamic -compatibility_version 1 -current_version 1 -macosx_version_min 10.11 -o $@ $<

%.so: %.o
	$(LD) -shared -o $@ $<

$(LIBS:.o=.so): CFLAGS := $(CFLAGS) -fpic
$(LIBS:.o=.so):

clean:
	$(RM) $(BIN) $(wildcard *.o) $(wildcard *.dylib) $(wildcard *.so)

test: $(BIN) $(LIBS)
	./$< add 13 7
	./$< mul 13 7
	! ./$< exp 13 7
	! ./$< add herp 7
	! ./$< mul 13 derp
	! ./$< add 13
	! ./$< add
	! ./$< add 13 7 42
	@mv -f binary_mul.$(SO) .binary_mul.$(SO)
	! ./$< mul 13 7
	@mv .binary_mul.$(SO) binary_mul.$(SO)

.PHONY: all clean test
