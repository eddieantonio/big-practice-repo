#include <assert.h>
#include <stdbool.h>

#include <unistd.h>
#include <signal.h>
#include <sys/time.h>

#include "gameloop.h"

static const suseconds_t ONE_SIXTIETH_OF_A_SECOND = 1000000 / 60;

static const struct itimerval frame_timer = {
    .it_interval = {
        .tv_usec = ONE_SIXTIETH_OF_A_SECOND
    },
    .it_value = {
        .tv_usec = ONE_SIXTIETH_OF_A_SECOND
    }
};

static frame_function_t frame_function = NULL;
static volatile bool should_continue = false;
static volatile frame_t elapsed_frames = 0;

/* Store the old sigaction. */
static struct sigaction old_action;

static void gameloop(int signal) {
    assert(signal == SIGALRM);
    frame_function(elapsed_frames);
    elapsed_frames++;
}

static void init_gameloop(frame_function_t func) {
    struct sigaction action = {
        .sa_handler = gameloop,
        .sa_mask = 0,
        .sa_flags = 0
    };

    frame_function = func;
    should_continue = true;

    /* Set signal handler. */
    sigaction(SIGALRM, &action, &old_action);
    /* Start the timer. */
    setitimer(ITIMER_REAL, &frame_timer, NULL);
}

frame_t gameloop_start(frame_function_t func) {
    init_gameloop(func);

    while (should_continue) {
        pause();
    }

    return elapsed_frames;
}

frame_t gameloop_stop() {
    struct itimerval null_timer = { .it_value = { 0 } };

    /* Cancel the timer. */
    setitimer(ITIMER_REAL, &null_timer, NULL);
    sigaction(SIGALRM, &old_action, NULL);

    if (frame_function == NULL) {
        return -1;
    }

    frame_t total_frames = elapsed_frames;

    /* Reset all variables. */
    frame_function = NULL;
    elapsed_frames = 0;
    should_continue = false;

    return total_frames;
}
