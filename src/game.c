#include <math.h>

#include <signal.h>
#include <curses.h>

#include "gameloop.h"
#include "game.h"
#include "sketchpad.h"
#include "line.h"

/* Constants! */

static const int SCREEN_WIDTH = 640;
static const int SCREEN_HEIGHT = 480;

static const coordinate SCREEN_CENTER = {
    .x = SCREEN_WIDTH / 2,
    .y = SCREEN_HEIGHT / 2
};

/** Duration of the loop. */
static const double DURATION = 5.0L; // Seconds
/** Length of the line. */
static const double LINE_LENGTH = 200; // pixels

/* Globals! */

static line* last_line = NULL;

/* Functions! */

static double progress(double frame) {
    return frame / (60 * DURATION);
}

static line line_coords(const coordinate *origin, double angle) {
    double half_x_disp = cos(angle) * LINE_LENGTH / 2.0;
    double half_y_disp = sin(angle) * LINE_LENGTH / 2.0;

    return (line) {
        .start = {
            .x = origin->x - half_x_disp,
            .y = origin->y - half_y_disp
        },
        .end = {
            .x = origin->x + half_x_disp,
            .y = origin->y + half_y_disp
        }
    };
}

static color_t to_rgb(uint8_t red, uint8_t green, uint8_t blue) {
    return red << 16 | green << 8 | blue;
}

/* Procedure from: http://www.rapidtables.com/convert/color/hsv-to-rgb.htm */
static color_t from_hsv(double hue, double saturation, double value) {
    double h = fmod(hue, 360),
           c = value * saturation,
           x = c * (1 - fabs(fmod(h / 60.0, 2) - 1)),
           m = value - c;

    double r, g, b;

    if (h < 60) {
        r = c, g = x, b = 0;
    } else if (h < 120) {
        r = x, g = c, b = 0;
    } else if (h < 180) {
        r = 0, g = c, b = x;
    } else if (h < 240) {
        r = 0, g = x, b = c;
    } else if (h < 300) {
        r = x, g = 0, b = c;
    } else {
        r = c, g = 0, b = x;
    }

    return to_rgb((r + m) * 255, (g + m) * 255, (b + m) * 255);
}

static void draw_line(line l) {
    static line line_storage;

    /* Copy the line and set the pointer. */
    line_storage = l;
    last_line = &line_storage;
    sketchpad_draw(l.start.x, l.start.y, l.end.x, l.end.y);
}

static void erase_line(line l) {
    sketchpad_erase(l.start.x, l.start.y, l.end.x, l.end.y);
}

static double rad_angle(double x) {
    return (2 * M_PI) * x;
}

static double deg_angle(double x) {
    return 360.0 * x;
}

static void erase_last_line() {
    if (last_line != NULL) {
        erase_line(*last_line);
    }
}

static char get_input() {
    return getch();
}

static void stahp(int signal) {
    /* Ends curses. */
    endwin();
    gameloop_stop();
}

/**
 * Runs on each frame.
 */
static void render_next(frame_t frame) {
    double angle = progress(frame);

    char in = get_input();
    if (in == 'q') {
        stahp(-1);
    }

    coordinate center = SCREEN_CENTER;
    center.x += 100.0 * cos(rad_angle(progress(frame)));

    erase_last_line();
    sketchpad_color(from_hsv(deg_angle(angle), 1.0, 1.0));
    draw_line(line_coords(&center, rad_angle(angle)));

    sketchpad_flush();
}

/** ncurses initialization. */
static void init_curses() {
    initscr();
    /* Do not echo characters typed to the terminal. */
    noecho();
    /* Do not block. */
    cbreak();

    keypad(stdscr, true);
    nodelay(stdscr, true);

    move(12, 4);
    printw("Welcome to lander!\n"
           "    Left/right arrow kets for rotation\n"
           "    Space for thrust.\n");
    refresh();
}

void start_game() {
    /* STAHP when given SIGINT; we only need the signal handler once, since
     * stahp() should exit gracefully. */
    signal(SIGINT, stahp);
    init_curses();

    gameloop_start(render_next);
}
