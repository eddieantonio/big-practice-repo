#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char test_string[] = "    good  morning   that's   a    nice tnetenba     ";

static char get_start(const char *input, int length) {
    for (int i = 0; i < length; i++) {
        if (input[i] != ' ') {
            return i;
        }
    }
    return length;
}

static char get_end(const char *input, int length) {
    for (int i = length - 1; i >= 0; i--) {
        if (input[i] != ' ') {
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

    int j = 0;
    for (int i = start; i < end; ) {
        /* Copy the current character. */
        if (input[i] != ' ') {
            buffer[j++] = input[i];
            i++;
        } else {
            buffer[j++] = ' ';
            /* Advance at least one character, skipping spaces. */
            while (input[i] == ' ') {
                i++;
            }
        }
    }
    buffer[j] = '\0';

    return buffer;
}

int main(int argc, char **argv) {
    char *cleaned = cleanup_string(test_string);
    printf("'%s'\n  = '%s'\n", test_string, cleaned);
    free(cleaned);

    return 0;
}
