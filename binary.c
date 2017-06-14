#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>


int main(int argc, char **argv) {
    /* TODO: accept three arguments like subcommands. */
    if (argc != 3) {
        return 2;
    }
    int a = atoi(argv[1]), b = atoi(argv[2]);

    void *lib = dlopen("binary_add.dylib", RTLD_LAZY);
    int (*binary_op)(int, int) = dlsym(lib, "binary_op");
    char* (*binary_sym)(void) = dlsym(lib, "binary_sym");

    int value = binary_op(a, b);
    printf("%d %s %d = %d\n", a, binary_sym(), b, value);

    dlclose(lib);
    return 0;
}
