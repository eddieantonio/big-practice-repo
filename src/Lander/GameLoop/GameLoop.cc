#include <cassert>
#include <cstdlib>

#include <signal.h>
#include <sys/time.h>

#include "Signal/Signal.h"

#include "Lander/GameLoop/GameLoop.h"

namespace {

Lander::GameLoop::GameLoop *installed_loop = 0;

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

}

namespace Lander {
namespace GameLoop {


GameLoop::GameLoop(Definition& def)
    : handler(def), shouldContinue(false), elapsedFrames(0), alarm(1.0 / 60)
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
    alarm.start();

    while (shouldContinue) {
        pause();
    }

    return *this;
}

GameLoop& GameLoop::stop()
{
    alarm.stop();
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
