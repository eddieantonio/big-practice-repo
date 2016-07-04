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

#include <cmath>
#include <sys/time.h>

#include "Signal/Alarm.h"

namespace Signal {

Alarm::Alarm(double seconds)
    : Alarm(
            (time_t) seconds,
            1000000 * fmod(seconds, 1.0)
        ) { }

Alarm::Alarm(time_t secs, μtime msecs)
    : started(false), seconds(secs), microseconds(msecs) { }

Alarm::~Alarm()
{
    stop();
}

Alarm& Alarm::start()
{
    if (started)
        return *this;

    struct itimerval timer = {
        { seconds, static_cast<suseconds_t>(microseconds)  },
        { seconds, static_cast<suseconds_t>(microseconds)  },
    };

    /* Start the timer. */
    setitimer(ITIMER_REAL, &timer, &oldTimer);
    started = true;

    return *this;
}

Alarm& Alarm::stop()
{
    if (!started)
        return *this;

    /* Cancel the timer. */
    setitimer(ITIMER_REAL, &oldTimer, nullptr);
    started = false;

    return *this;
}

}
