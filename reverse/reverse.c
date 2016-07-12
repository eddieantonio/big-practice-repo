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
#include <assert.h>

#include "debugbreak.h"

static void swap (char *a, char *b) {
    char c = *a;
    *a = *b;
    *b = c;
}

static void reverse_utf8_seq(char *p, unsigned multibyte_len) {
    switch (multibyte_len) {
        case 2:
            swap(&p[0], &p[1]);
            return;
        case 3:
            swap(&p[0], &p[2]);
            return;
        case 4:
            swap(&p[0], &p[3]);
            swap(&p[1], &p[2]);
            return;
        default:
            debug_break();
            assert(false && "Invalid byte length");
    }
}

static int utf8_following_bytes(const char c) {
    if ((c & 0x80)  == 0) {
        return 0;
    } else if ((c & 0xE0) == 0xC0) {
        return 1;
    } else if ((c & 0xF0) == 0xE0) {
        return 2;
    } else if ((c & 0xF8) == 0xF0) {
        return 3;
    } else if ((c & 0xC0) == 0x80) {
        /* UTF-8 continuation byte. */
        return 0;
    }
    assert(false && "Invalid UTF-8 byte");
}

bool reverse(char *p, size_t max_len) {
    char *q = p + strnlen(p, max_len) - 1;

    unsigned bytes_until_swap = 0;
    unsigned multibyte_len = 0;
    unsigned backwards_len = 0;

    while (p < q) {
        /* Unreverse a UTF-8 sequence found traversing forward. */
        if (bytes_until_swap) {
            if (--bytes_until_swap == 0) {
                /* The bytes were swapped to the end. */
                reverse_utf8_seq(q + multibyte_len, multibyte_len);
            }
        } else {
            bytes_until_swap = utf8_following_bytes(*p);
            multibyte_len = bytes_until_swap + 1;
        }

        swap(p, q);

        /* Unravel a UTF-8 sequence found traversing backwards. */
        backwards_len = utf8_following_bytes(*p) + 1;
        if (backwards_len > 1) {
            reverse_utf8_seq(p - backwards_len, backwards_len);
        }

        p++;
        q--;
    }

    /* We may have surpassed the midpoint, so unswap the UTF-8 sequence we're in the
     * middle of. */
    if (bytes_until_swap) {
        reverse_utf8_seq(q - --bytes_until_swap, multibyte_len);
    }

    return true;
}
