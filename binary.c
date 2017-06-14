#include <stdio.h>
#include <stdlib.h>

int binary_op(int, int);

int main(int argc, char **argv) {
    if (argc != 3) {
        return 2;
    }
    /* TODO: accept three arguments like subcommands. */

    int value = binary_op(atoi(argv[1]), atoi(argv[2]));
    printf("%d\n", value);

    return 0;
}
