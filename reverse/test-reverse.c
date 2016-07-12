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

    /* 2-byte form. */
    char xi[] = "Ξ";
    assert_true(sizeof(xi) == 2 + 1);
    assert_true(reverse(xi, sizeof(xi)));
    assert_str_eq(xi, "Ξ", sizeof(xi));

    /* 3-byte form, */
    char multiocular[] = "ꙮ";
    assert_true(sizeof(multiocular) == 3 + 1);
    assert_true(reverse(multiocular, sizeof(multiocular)));
    assert_str_eq(multiocular, "ꙮ", sizeof(multiocular));

    /* 4-byte form. */
    char poop[] = "💩";
    assert_true(reverse(poop, sizeof(poop)));
    assert_str_eq(poop, "💩", sizeof(poop));


    char big_test[] = "ɼ∀🌜E🌛∀ɹ";
    assert_true(reverse(big_test, sizeof(big_test)));
    printf("test: ``%s''\n", big_test);
    assert_str_eq(big_test, "ɼ∀🌜E🌛∀ɹ", sizeof(big_test));

    assert_end();
}
