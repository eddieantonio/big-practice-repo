/*
 * Copyright (C) 2016 Eddie Antonio Santos <easantos@ualberta.ca>
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

#include <fcntl.h>
#include <unistd.h>
#include <wordexp.h>
#include <sys/stat.h>
#include <sys/mman.h>

static const char* get_dedication();

int main(int argc, const char *argv[]) {
    const char *dedication;

    if (argc > 1) {
        dedication = argv[1];
    } else {
        dedication = get_dedication();
    }

    printf("Hello, %s!\n", dedication);

    return 0;
}

/**
 * @returns The null-terminated contents of the named file.
 * If this fails for any reason, returns NULL.
 */
static const char* slurp_file(const char* filename) {
    wordexp_t expansion;
    struct stat statbuf;
    int fd = -1;
    off_t filesize;
    char *memory;

    wordexp(filename, &expansion, 0);
    fd = open(expansion.we_wordv[0], O_RDONLY);
    wordfree(&expansion);

    /* Open failed. */
    if (fd < 0) {
        return NULL;
    }

    /* Get file size, if available. */
    if (fstat(fd, &statbuf) < 0) {
        close(fd);
        return NULL;
    }

    filesize = statbuf.st_size;

    /* Check if there's actual content. */
    if (filesize < 1) {
        close(fd);
        return NULL;
    }

    /* Map the file and immediately close it. */
    memory = mmap(
            NULL,
            filesize + 1,           /* Need to write a null-terminator, possibly
                                       one byte after the entire file. */
            PROT_READ|PROT_WRITE,   /* Need to write the zero-terminator. */
            MAP_PRIVATE,            /* On OS X, can't use MAP_SHARED with a
                                       read-only file descriptor, so we must
                                       use MAP_PRIVATE. */
            fd,
            0);
    close(fd);

    /* Could not map file. */
    if (memory == MAP_FAILED) {
        return NULL;
    }

    /* Ensure the last byte is a zero-terminator. */
    if (memory[filesize - 1] == '\n') {
        /* Replace the last newline with the zero terminator, if it exists. */
        memory[filesize - 1] = '\0';
    } else {
        /* Go **OUTSIDE** the buffer, and replace it with a null-terminator. */
        memory[filesize] = '\0';
    }

    return memory;
}

static const char* get_dedication() {
    const char *dedication;

    if ((dedication = getenv("DEFAULT_HELLO_DEDICATION")) != NULL) {
        return dedication;
    } else if ((dedication = slurp_file("~/.hellorc")) != NULL) {
        return dedication;
    } else if ((dedication = slurp_file("/etc/hello.conf")) != NULL) {
        return dedication;
    } else {
        return "World";
    }
}
