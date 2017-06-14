#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <dlfcn.h>

static const char *program_name;

static void usage(FILE *output) {
    fprintf(output, "Usage:\n");
    fprintf(output, "  %s (add|mul) <i> <j>\n", program_name);
}

static void usage_error(const char *msg, ...) {
    if (msg != NULL) {
        fprintf(stderr, "%s: ", program_name);
        va_list ap;
        va_start(ap, msg);
        vfprintf(stderr, msg, ap);
        va_end(ap);
        fprintf(stderr, "\n");
    }
    usage(stderr);
    exit(2);
}

static int int_or_die(const char* str) {
    char *end;
    long answer = strtol(str, &end, 10);
    if (str == end) {
        usage_error("invalid integer: '%s'", str);
    }
    return answer;
}

int main(int argc, char **argv) {
    program_name = argv[0];
    /* TODO: accept three arguments like subcommands. */
    if (argc != 4) {
        usage_error("requires exactly 3 arguments");
    }
    int a = int_or_die(argv[2]), b = int_or_die(argv[3]);
    const char *library;

    if (strncmp(argv[1], "add", 4) == 0) {
        library = "binary_add.dylib";
    } else if (strncmp(argv[1], "mul", 4) == 0) {
        library = "binary_mul.dylib";
    } else {
        usage_error("invalid subcommand: '%s'", argv[1]);
    }

    void *lib = dlopen(library, RTLD_LAZY);
    int (*binary_op)(int, int) = dlsym(lib, "binary_op");
    char* (*binary_sym)(void) = dlsym(lib, "binary_sym");

    int value = binary_op(a, b);
    printf("%d %s %d = %d\n", a, binary_sym(), b, value);

    dlclose(lib);
    return 0;
}
