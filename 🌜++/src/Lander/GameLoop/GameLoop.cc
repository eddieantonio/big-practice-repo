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
