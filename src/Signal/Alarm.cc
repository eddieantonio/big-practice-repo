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
    setitimer(ITIMER_REAL, &oldTimer, NULL);
    started = false;

    return *this;
}

}
