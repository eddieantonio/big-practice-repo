#include <math.h>
#include <stdbool.h>

#include "sketchpad.h"

static const char default_command[] = "java -jar Sketchpad.jar";

static bool safe_to_close = false;
static FILE *sketchpad = NULL;
static sketchpad_flags_t options = SKETCHPAD_FLUSH_ON_CONTROL;

static color_t current_color = 0x000000;
static color_t last_color = 0x000000;

/** Register a stream as a shutdown process. */
FILE* sketchpad_register(FILE * process) {
    FILE *last_assignment = sketchpad;
    sketchpad = process;
    return last_assignment;
}

FILE* sketchpad_register_default() {
    safe_to_close = true;
    return sketchpad_register(popen(default_command, "w"));
}

sketchpad_flags_t sketchpad_set_flags(sketchpad_flags_t new_flags) {
    sketchpad_flags_t old_flags = options;
    options = new_flags;
    return old_flags;
}

/** Any outstanding changes are written immediately to sketchpad. */
int sketchpad_flush() {
    return fflush(sketchpad);
}

void sketchpad_draw(double x1, double y1, double x2, double y2) {
    fprintf(sketchpad, "drawSegment %ld %ld %ld %ld\n",
            lround(x1), lround(y1), lround(x2), lround(y2));
    (options & SKETCHPAD_FLUSH_ON_DRAW) && sketchpad_flush();
}

void sketchpad_erase(double x1, double y1, double x2, double y2){
    fprintf(sketchpad, "eraseSegment %ld %ld %ld %ld\n",
            lround(x1), lround(y1), lround(x2), lround(y2));
    (options & SKETCHPAD_FLUSH_ON_ERASE) && sketchpad_flush();
}

color_t sketchpad_color(color_t color){
    const uint8_t red = (color & 0xFF0000) >> 16,
                green = (color & 0x00FF00) >> 8,
                 blue = (color & 0x0000FF);

    last_color = current_color;
    current_color = color;
    fprintf(sketchpad, "setColor %d %d %d\n", red, green, blue);

    (options & SKETCHPAD_FLUSH_ON_COLOR) && sketchpad_flush();

    return last_color;
}

/** Restore the last color. */
color_t sketchpad_restore_last_color() {
    return sketchpad_color(last_color);
}

/** Send the pause command. */
void sketchpad_pause(double seconds) {
    fprintf(sketchpad, "pause %lf\n", seconds);
    (options & SKETCHPAD_FLUSH_ON_CONTROL) && sketchpad_flush();
}

/** Ends sketchpad communication; further commands are invalid. */
void sketchpad_end() {
    fprintf(sketchpad, "end\n");
    (options & SKETCHPAD_FLUSH_ON_CONTROL) && sketchpad_flush();
}

/** Closes sketchpad. */
int sketchpad_shutdown() {
    sketchpad_end();
    if (safe_to_close) {
        return pclose(sketchpad);
    }

    return -1;
}
