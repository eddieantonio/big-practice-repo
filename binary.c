#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dlfcn.h>


int main(int argc, char **argv) {
    /* TODO: accept three arguments like subcommands. */
    if (argc != 4) {
        return 2;
    }
    int a = atoi(argv[2]), b = atoi(argv[3]);
    const char *library;

    if (strncmp(argv[1], "add", 3) == 0) {
        library = "binary_add.dylib";
    } else if (strncmp(argv[1], "mul", 3) == 0) {
        library = "binary_mul.dylib";
    } else {
        return 2;
    }

    void *lib = dlopen(library, RTLD_LAZY);
    int (*binary_op)(int, int) = dlsym(lib, "binary_op");
    char* (*binary_sym)(void) = dlsym(lib, "binary_sym");

    int value = binary_op(a, b);
    printf("%d %s %d = %d\n", a, binary_sym(), b, value);

    dlclose(lib);
    return 0;
}
