/*
 * Copyright (C) 2017 Eddie Antonio Santos <easantos@ualberta.ca>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
