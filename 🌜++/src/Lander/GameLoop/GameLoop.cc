#include <cassert>
#include <cstdlib>

#include <signal.h>

#include "Signal/Signal.h"
#include "Signal/Mask.h"

#include "Lander/GameLoop/GameLoop.h"

namespace {

using Signal::Mask::Manner::Replace;
using Lander::GameLoop::GameLoop;

sigset_t allSignals()
{
    sigset_t mask;
    sigfillset(&mask);
    return mask;
}

class AlarmHandler : public Signal::SignalAction
{
public:
    AlarmHandler(GameLoop& loop) : loop(loop) { }

    virtual void handle(int signalID)
    {
        assert(signalID == SIGALRM);

        Signal::Mask blockAll(Replace, allSignals());
        loop.doFrame();
    }
private:
    GameLoop& loop;
};

}

namespace Lander {
namespace GameLoop {

GameLoop::GameLoop(Definition& def)
    : handler(def), shouldContinue(false), elapsedFrames(0), alarm(1.0 / 60)
{
}

GameLoop::~GameLoop()
{
    stop();
}

GameLoop& GameLoop::start()
{
    shouldContinue = true;

    AlarmHandler onAlarm(*this);
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
    handler.eachFrame(*this);
    return elapsedFrames++;
}

}
}
