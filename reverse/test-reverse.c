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
#include <stdbool.h>
#include <assert.h>
#include <string.h>

bool reverse(char *p, size_t max_len);

#define RED     "\x1b[31m"
#define GREEN   "\x1b[32m"
#define RESET   "\x1b[m"

#define streq(p, q, n) (strncmp((p), (q), (n)) == 0)

struct assert_status {
    bool passed;
    const char *assertion;
    struct assert_status *next;
};

static struct {
    unsigned tests_passed, tests_failed, test_progress;
    struct assert_status *statuses;
} assert_ctx = { 0, 0, 0, NULL };

#define assert_init()                                                                \
    struct assert_status *current = assert_ctx.statuses = & (struct assert_status) { \
        .passed = true,                                                              \
        .assertion = NULL,                                                           \
        .next = NULL                                                                 \
    }

#define assert_true(expression)               \
    assert_ctx.test_progress++;               \
    current->next = &(struct assert_status) { \
        .assertion = "" #expression "",       \
        .passed = false,                      \
        .next = NULL                          \
    };                                        \
    current = current->next;                  \
    current->passed = (bool) (expression);    \
    if (current->passed) {                    \
        assert_ctx.tests_passed++;            \
    } else {                                  \
        assert_ctx.tests_failed++;            \
    }


int main(int argc, const char *argv[]) {
    assert_init();

    char empty[] = "";
    assert_true(strnlen(empty, 1) == 0);
    assert_true(reverse(empty, 0));
    assert_true(streq(empty, "", 1));

    assert_true(reverse(empty, 512));
    assert_true(streq(empty, "", 1));

    char hello[] = "hello";
    assert_true(reverse(hello, 5));
    assert_true(streq("olleh", hello, 5));

    /* TODO: UTF-8 tests. */

    for (current = assert_ctx.statuses->next; current != NULL; current = current->next) {
        if (!current->passed) {
            fprintf(stderr, RED "FAILED: " RESET "%s\n", current->assertion);
        }
    }

    if (assert_ctx.tests_failed == 0) {
        fprintf(stderr, GREEN "Ok! %u tests passed!" RESET "\n", assert_ctx.tests_passed);
        return 0;
    } else {
        fprintf(stderr, RED "%u out of %u tests failed" RESET "\n",
                assert_ctx.tests_failed, assert_ctx.test_progress);
    }
}
