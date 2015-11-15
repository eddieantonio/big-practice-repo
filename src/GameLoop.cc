#include <cassert>
#include <cstdlib>

#include <signal.h>
#include <sys/time.h>

#include "Lander/GameLoop/GameLoop.h"

static void disable_gameloop();
static void init_gameloop();

static Lander::GameLoop::GameLoop *installed_loop = 0;

namespace Lander {
namespace GameLoop {


GameLoop::GameLoop(Definition& def)
    : handler(def), shouldContinue(false), elapsedFrames(0)
{
    if (installed_loop != 0) {
        /* Should not have constructed twice! */
        abort();
    }

    installed_loop = this;
}

GameLoop::~GameLoop()
{
    stop();

    assert(installed_loop == this);
    installed_loop = 0;
}

GameLoop& GameLoop::start()
{
    init_gameloop();

    shouldContinue = true;
    while (shouldContinue) {
        pause();
    }

    return *this;
}

GameLoop& GameLoop::stop()
{
    disable_gameloop();
    shouldContinue = false;

    return *this;
}

FrameCounter GameLoop::doFrame()
{
    handler.eachFrame(*this, elapsedFrames);
    return elapsedFrames++;
}

}
}

/* Unix/C stuff below: */

static const suseconds_t ONE_SIXTIETH_OF_A_SECOND = 1000000 / 60;

static const struct itimerval frame_timer = {
    { 0, ONE_SIXTIETH_OF_A_SECOND },
    { 0, ONE_SIXTIETH_OF_A_SECOND },
};

/* Store the old sigaction. */
static struct sigaction old_action;
static sigset_t old_mask;

static void block_signals() {
    sigset_t all_signals;
    sigfillset(&all_signals);

    sigprocmask(SIG_BLOCK, &all_signals, &old_mask);
}

static void unblock_signals() {
    sigprocmask(SIG_SETMASK, &old_mask, NULL);
}

static void on_alarm(int sig_id)
{
    assert(sig_id == SIGALRM);
    assert(installed_loop != 0);

    block_signals();
    installed_loop->doFrame();
    unblock_signals();
}

static void init_gameloop() {
    struct sigaction action = {
        { on_alarm }, 0, 0
    };

    /* Set signal handler. */
    sigaction(SIGALRM, &action, &old_action);
    /* Start the timer. */
    setitimer(ITIMER_REAL, &frame_timer, NULL);
}

static void disable_gameloop() {
    struct itimerval null_timer = { { 0 }, { 0 } };

    /* Cancel the timer. */
    setitimer(ITIMER_REAL, &null_timer, NULL);
    sigaction(SIGALRM, &old_action, NULL);
}
