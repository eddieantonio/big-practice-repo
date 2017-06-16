BIN = binary
LIBS = $(addsuffix .$(SO), binary_add binary_mul)

CFLAGS = -std=c11 -Wall -Werror
LDLIBS = -ldl

ifeq ($(shell uname -s),Darwin)
SO = dylib
else
SO = so
endif

all: $(BIN) $(LIBS)

$(BIN): LDFLAGS := $(LDFLAGS) -Wl,-rpath=$(PWD)
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
	@chmod a+rx binary_mul.$(SO)
	./$< add 13 7
	./$< mul 13 7
	! ./$< exp 13 7
	! ./$< add herp 7
	! ./$< mul 13 derp
	! ./$< add 13
	! ./$< add
	! ./$< add 13 7 42
	@chmod a-rx binary_mul.$(SO)
	! ./$< mul 13 7

.PHONY: all clean test
