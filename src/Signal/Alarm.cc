#include <cmath>
#include <sys/time.h>

#include "Signal/Alarm.h"

namespace Signal {

Alarm::Alarm()
    : Alarm(1.0) { }

Alarm::Alarm(double seconds)
    : Alarm(
            (time_t) seconds,
            1000000 * fmod(seconds, 1.0)
        ) { }

Alarm::Alarm(time_t secs, μtime msecs)
    : started(false), seconds(secs), microseconds(msecs) { }

Alarm& Alarm::start()
{
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
    /* Cancel the timer. */
    setitimer(ITIMER_REAL, &oldTimer, NULL);
    started = false;

    return *this;
}

}
