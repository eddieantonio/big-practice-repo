#ifndef SKETCHPAD_H
#define SKETCHPAD_H

#include <stdio.h>
#include <stdint.h>

typedef uint_least32_t color_t;

/** Register a stream as a shutdown process. */
FILE* sketchpad_register(FILE * process);
/** Register the default stream as a shutdown process. */
FILE* sketchpad_register_default();

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
