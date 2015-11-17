#include <cassert>
#include <cstdlib>

#include <signal.h>
#include <sys/time.h>

#include "Signal/Signal.h"

#include "Lander/GameLoop/GameLoop.h"

namespace {

Lander::GameLoop::GameLoop *installed_loop = 0;

/* Unix/C stuff below: */

static const suseconds_t ONE_SIXTIETH_OF_A_SECOND = 1000000 / 60;

static const struct itimerval frame_timer = {
    { 0, ONE_SIXTIETH_OF_A_SECOND },
    { 0, ONE_SIXTIETH_OF_A_SECOND },
};

/* Store the old sigaction. */
static sigset_t old_mask;

static void block_signals() {
    sigset_t all_signals;
    sigfillset(&all_signals);

    sigprocmask(SIG_BLOCK, &all_signals, &old_mask);
}

static void unblock_signals() {
    sigprocmask(SIG_SETMASK, &old_mask, NULL);
}

class AlarmHandler : public Signal::SignalAction
{
    virtual void handle(int signal)
    {
        assert(signal == SIGALRM);
        assert(installed_loop != 0);

        block_signals();
        installed_loop->doFrame();
        unblock_signals();
    }
};

void start_alarm() {
    /* Start the timer. */
    setitimer(ITIMER_REAL, &frame_timer, NULL);
}

void stop_alarm() {
    /* Cancel the timer. */
    struct itimerval null_timer = { { 0 }, { 0 } };
    setitimer(ITIMER_REAL, &null_timer, NULL);
}

}

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
    shouldContinue = true;

    AlarmHandler onAlarm;
    Signal::Signal handleAlarm(SIGALRM, onAlarm);
    start_alarm();

    while (shouldContinue) {
        pause();
    }

    return *this;
}

GameLoop& GameLoop::stop()
{
    stop_alarm();
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

