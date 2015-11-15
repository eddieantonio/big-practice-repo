# Get absolute path to containing directory: http://stackoverflow.com/a/324782
TOP := $(dir $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))
LOCAL_INCLUDE_DIR := $(realpath $(TOP)include)

NAME=🌜++
VERSION=0.1.0

# Special compiler flags
CXXFLAGS = -std=c++11 -Wall -pedantic
CPPFLAGS = -MMD -I$(LOCAL_INCLUDE_DIR)
LDFLAGS =
LDLIBS = -lm -lcurses

BIN = $(NAME)
SRCS = $(wildcard src/*.cc)
OBJS = $(patsubst src/%.cc,build/%.o,$(SRCS))
DEPS := $(patsubst src/%.cc,build/%.d,$(SRCS))

DISTNAME = $(NAME)-$(VERSION)

# Standard targets.
all: $(BIN)

# Generates the binary from the objects
$(BIN): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

# Generates object files in a seperate directory.
build/%.o: src/%.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -o $@ -c $^

# Include dependencies from -d
-include $(DEPS)

$(DISTNAME).tar.gz: $(SRCS) Makefile
	git archive HEAD --prefix=$(DISTNAME)/ | gzip > $@

clean:
	-$(RM) $(BIN)
	-$(RM) build/*.o
	-$(RM) build/*.d
	-$(RM) $(DISTNAME).tar.gz

dist: $(DISTNAME).tar.gz

test: $(BIN)
	./$(BIN)

mega-test: $(BIN)
	./$(BIN) test

.PHONY: all clean test mega-test
