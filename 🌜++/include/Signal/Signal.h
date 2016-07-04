#ifndef SIGNAL_SIGNAL_H
#define SIGNAL_SIGNAL_H

#include <signal.h>

namespace Signal {

class SignalAction
{
public:
    virtual ~SignalAction() {}
    virtual void handle(int signal) = 0;
};

/**
 * RAII class that wraps a POSIX signal.
 */
class Signal {
public:
    Signal(int type, SignalAction& handler);
    ~Signal();

protected:
    const SignalAction& handler;
    const int registeredSignal;

    struct sigaction oldAction;
};

}

#endif /* SIGNAL_SIGNAL_H */
