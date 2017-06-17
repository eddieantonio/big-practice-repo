# Copyright (C) 2017 Eddie Antonio Santos <easantos@ualberta.ca>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

BIN = binary
LIBS = $(addsuffix .$(SO), binary_add binary_mul)

CFLAGS = -std=c11 -Wall -Werror
LDLIBS = -ldl

ifeq ($(shell uname -s),Darwin)
SO = dylib
else
SO = so
LDEXTRA = -rdynamic -Wl,-rpath=$(PWD)
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
