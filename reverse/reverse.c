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

#include <string.h>
#include <stdbool.h>

static void swap (char *a, char *b) {
    char c = *a;
    *a = *b;
    *b = c;
}

bool reverse(char *p, size_t max_len) {
    char *q = p + strnlen(p, max_len) - 1;

    while (p < q) {
        swap(p, q);
        p++;
        q--;
    }

    return true;
}
