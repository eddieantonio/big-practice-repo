#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <err.h>
#include <sysexits.h>

#include <dlfcn.h>

#ifdef __APPLE__
#define SHARED_OBJECT(name)    (name ".dylib")
#elif __linux__
#define SHARED_OBJECT(name)    (name ".so")
#else
#   error "Unknown shared object format"
#endif

static void usage(FILE *output);
static void usage_error(const char *msg, ...);
static int int_or_die(const char* str);

static const char *program_name = __FILE__;


int main(int argc, char **argv) {
    if (argc != 4) {
        usage_error("requires exactly 3 arguments");
    }

    int a = int_or_die(argv[2]),
        b = int_or_die(argv[3]);

    const char *library;
    if (strncmp(argv[1], "add", 4) == 0) {
        library = SHARED_OBJECT("binary_add");
    } else if (strncmp(argv[1], "mul", 4) == 0) {
        library = SHARED_OBJECT("binary_mul");
    } else {
        usage_error("invalid subcommand: '%s'", argv[1]);
    }

    void *lib = dlopen(library, RTLD_LAZY);
    if (lib == NULL) {
        errx(EX_UNAVAILABLE, "%s", dlerror());
    }
    int (*binary_op)(int, int) = dlsym(lib, "binary_op");
    char* (*binary_sym)(void) = dlsym(lib, "binary_sym");
    if ((binary_op == NULL) || (binary_sym == NULL)) {
        errx(EX_UNAVAILABLE, "%s", dlerror());
    }

    int value = binary_op(a, b);
    printf("%d %s %d = %d\n", a, binary_sym(), b, value);

    dlclose(lib);
    return 0;
}


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
    exit(EX_USAGE);
}


static int int_or_die(const char* str) {
    char *end;
    long answer = strtol(str, &end, 10);
    if (str == end) {
        usage_error("invalid integer: '%s'", str);
    }
    return answer;
}


/* Automatically sets the program name variable. */
__attribute__ ((constructor))
void _set_program_name(int argc, char *argv[]) {
    program_name = argv[0];
    if (strncmp(program_name, "./", 2) == 0) {
        /* Omit the dot-slash. */
        program_name += 2;
    }
}
