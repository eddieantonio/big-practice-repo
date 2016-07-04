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

#ifndef SKETCHPAD_H
#define SKETCHPAD_H

#include <stdio.h>
#include <stdint.h>

/** All colors are specified using this: */
typedef uint_least32_t color_t;

/** Option flags. */
typedef enum {
    SKETCHPAD_FLUSH_ON_CONTROL  = 0x01,
    SKETCHPAD_FLUSH_ON_DRAW     = 0x02,
    SKETCHPAD_FLUSH_ON_ERASE    = 0x04,
    SKETCHPAD_FLUSH_ON_COLOR    = 0x08,
} sketchpad_flags_t;

/** Register a stream as a shutdown process. */
FILE* sketchpad_register(FILE * process);
/** Register the default stream as a shutdown process. */
FILE* sketchpad_register_default();

/** Set option flags. */
sketchpad_flags_t sketchpad_set_flags(sketchpad_flags_t new_flags);

/** Any outstanding changes are written immediately to sketchpad. */
int sketchpad_flush();

/** Draw a line */
void sketchpad_draw(double x1, double y1, double x2, double y2);
/** Erase a line */
void sketchpad_erase(double x1, double y1, double x2, double y2);

/** Change the draw color; previous color is saved. */
color_t sketchpad_color(color_t color);
/** Restore the last color. */
color_t sketchpad_restore_last_color();

/** Send the pause command. */
void sketchpad_pause(double seconds);
/** Ends sketchpad communication; further commands are invalid. */
void sketchpad_end();

/** Closes sketchpad. */
int sketchpad_shutdown();

#endif /* SKETCHPAD_H */
