#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static char test_string[] = "    good  morning   that's   a    nice tnetenba     ";

static char get_start(const char *input, int length) {
    for (int i = 0; i < length; i++) {
        if (!isspace(input[i])) {
            return i;
        }
    }
    /* Did not find start. */
    return length;
}

static char get_start_no_len(const char *input) {
    for (int i = 0;; i++) {
        if (!isspace(input[i])) {
            return i;
        }
    }
}


static char get_end(const char *input, int length) {
    for (int i = length - 1; i >= 0; i--) {
        if (!isspace(input[i])) {
            return i + 1;
        }
    }
    return 0;
}

static char* cleanup_string(const char *input) {
    const int length = strlen(input);
    const int start = get_start(input, length);
    const int end = get_end(input, length);

    const int clean_length = end - start;
    char *buffer = malloc(clean_length + 1);

    char *dest = buffer;
    for (int i = start; i < end; ) {
        int offset;
        if (isspace(input[i])) {
            *(dest++) = ' ';
            offset = get_start_no_len(input + i);
        } else {
            /* Copy the current character. */
            *(dest++) = input[i];
            offset = 1;
        }

        i += offset;
    }
    *dest = '\0';

    return buffer;
}

int main(int argc, char **argv) {
    char *cleaned = cleanup_string(test_string);
    printf("'%s'\n  = '%s'\n", test_string, cleaned);
    free(cleaned);

    return 0;
}
