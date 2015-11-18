#ifndef SIGNAL_ALARM_H
#define SIGNAL_ALARM_H

#include <ctime>
#include <sys/time.h>

namespace Signal {

class Alarm {
    typedef unsigned long μtime;
public:
    Alarm(double seconds);
    Alarm(time_t secs, μtime msecs);
    ~Alarm();

    Alarm& start();
    Alarm& stop();

protected:
    bool started;
    const time_t seconds;
    const μtime microseconds;
    struct itimerval oldTimer;
};

}
#endif /* SIGNAL_ALARM_H */
