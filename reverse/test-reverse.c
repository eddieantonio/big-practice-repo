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
#include <assert.h>
#include <string.h>
#include "assert-framework.h"

bool reverse(char *p, size_t max_len);

int main(int argc, const char *argv[]) {
    assert_init();

    char empty[] = "";
    assert_true(strnlen(empty, 1) == 0);
    assert_true(reverse(empty, 0));
    assert_str_eq(empty, "", 1);

    assert_true(reverse(empty, 512));
    assert_str_eq(empty, "", 1);

    char hello[] = "hello";
    assert_true(reverse(hello, 5));
    assert_str_eq("olleh", hello, 5);

    /* TODO: UTF-8 tests. */

    assert_end();
}
