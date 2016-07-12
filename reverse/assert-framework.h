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

#ifndef ASSERT_FRAMEWORK_H
#define ASSERT_FRAMEWORK_H

#include <stdbool.h>
#include <stddef.h>

#define _ANSI_RED     "\x1b[31m"
#define _ANSI_GREEN   "\x1b[32m"
#define _ANSI_RESET   "\x1b[m"

struct assert_status {
    bool passed;
    const char *assertion;
    struct assert_status *next;
};

static struct {
    unsigned tests_passed, tests_failed, test_progress;
    struct assert_status *statuses;
} assert_ctx = { 0, 0, 0, NULL };

#define assert_init()                                    \
    struct assert_status *assert_current =               \
        assert_ctx.statuses = & (struct assert_status) { \
            .passed = true,                              \
            .assertion = NULL,                           \
            .next = NULL                                 \
        }

#define assert_end()                                                            \
    for (assert_current = assert_ctx.statuses->next;                            \
            assert_current != NULL;                                             \
            assert_current = assert_current->next) {                            \
        if (!assert_current->passed) {                                          \
            fprintf(stderr, _ANSI_RED "FAILED: " _ANSI_RESET "%s\n",            \
                    assert_current->assertion);                                 \
        }                                                                       \
    }                                                                           \
    if (assert_ctx.tests_failed == 0) {                                         \
        fprintf(stderr, _ANSI_GREEN "Ok! %u tests passed!" _ANSI_RESET "\n",    \
                assert_ctx.tests_passed);                                       \
        return 0;                                                               \
    } else {                                                                    \
        fprintf(stderr, _ANSI_RED "%u out of %u tests failed" _ANSI_RESET "\n", \
                assert_ctx.tests_failed, assert_ctx.test_progress);             \
    }

#define assert_true(expression)                      \
    assert_ctx.test_progress++;                      \
    assert_current->next = &(struct assert_status) { \
        .assertion = "" #expression "",              \
        .passed = false,                             \
        .next = NULL                                 \
    };                                               \
    assert_current = assert_current->next;           \
    assert_current->passed = (bool) (expression);    \
    if (assert_current->passed) {                    \
        assert_ctx.tests_passed++;                   \
    } else {                                         \
        assert_ctx.tests_failed++;                   \
    }

#define assert_str_eq(a, b, n) \
    assert_true(strncmp(a, b, n) == 0)

#endif
