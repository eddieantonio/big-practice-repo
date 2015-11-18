#include <cassert>
#include <cstdlib>

#include <signal.h>

#include "Signal/Signal.h"
#include "Signal/Mask.h"

#include "Lander/GameLoop/GameLoop.h"

namespace {

using Signal::Mask::Manner::Replace;

Lander::GameLoop::GameLoop *installedLoop = 0;

sigset_t allSignals()
{
    sigset_t mask;
    sigfillset(&mask);
    return mask;
}

class AlarmHandler : public Signal::SignalAction
{
    virtual void handle(int signalID)
    {
        assert(signalID == SIGALRM);
        assert(installedLoop != 0);

        Signal::Mask blockAll(Replace, allSignals());
        installedLoop->doFrame();
    }
};

}

namespace Lander {
namespace GameLoop {

GameLoop::GameLoop(Definition& def)
    : handler(def), shouldContinue(false), elapsedFrames(0), alarm(1.0 / 60)
{
    if (installedLoop != 0) {
        /* Should not have constructed twice! */
        abort();
    }

    installedLoop = this;
}

GameLoop::~GameLoop()
{
    stop();

    assert(installedLoop == this);
    installedLoop = 0;
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
