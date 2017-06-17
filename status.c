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

/**
 * Small utility that returns the status that is passed as the first command
 * line argument (as a decimal number).
 */

#include <stdlib.h>
#include <stdio.h>

int main(int argc, const char* argv[]) {
    if (argc < 2) {
        return 0;
    }

    const char *program = argv[0];
    const char *arg = argv[1];
    char *end = NULL;
    long status = strtol(arg, &end, 10);

    if (end == arg || *end != '\0') {
        fprintf(stderr, "%s: warning: invalid status code: `%s`\n",
                program, arg);
    }

    if (status < 0) {
        fprintf(stderr, "%s: warning: status should be positive. Instead got: `%ld`\n",
                program, status);
    } else if (status > 0xFF) {
        fprintf(stderr, "%s: warning: status too large: `%ld`\n",
                program, status);
    }

    return status & 0xFF;
}
