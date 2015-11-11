# Get absolute path to containing directory: http://stackoverflow.com/a/324782
TOP := $(dir $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST)))
LOCAL_INCLUDE_DIR := $(realpath $(TOP)include)

NAME=🌜
VERSION=0.1.0

# Special compiler flags
CFLAGS = -std=c11 -Wall -pedantic
CPPFLAGS = -MMD -I$(LOCAL_INCLUDE_DIR)
LDFLAGS =
LDLIBS = -lm

BIN = $(NAME)
SRCS = $(wildcard src/*.c)
OBJS = $(patsubst %.c,%.o,$(SRCS))
DEPS := $(patsubst %.c,%.d,$(SRCS))

DISTNAME = $(NAME)-$(VERSION)

# Standard targets.
all: $(BIN)

# Generates the binary from the objects
$(BIN): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

# Include dependencies from -d
-include $(DEPS)

$(DISTNAME).tar.gz: $(SRCS) Makefile
	git archive HEAD --prefix=$(DISTNAME)/ | gzip > $@

clean:
	-$(RM) $(BIN)
	-$(RM) src/*.o
	-$(RM) src/*.d
	-$(RM) $(DISTNAME).tar.gz

dist: $(DISTNAME).tar.gz

test: $(BIN)
	./$(BIN)

mega-test: $(BIN)
	./$(BIN) test

.PHONY: all clean test mega-test
