#include <cassert>

#include "Signal/Signal.h"

#include <signal.h>

namespace {

Signal::SignalAction* signals[SIGUSR2 + 1];

void globalHandler(int signal, siginfo_t *info, void *data)
{
    assert(signals[signal] != 0);
    signals[signal]->handle(signal);
}

}

namespace Signal {

Signal::Signal(int type, SignalAction& handler)
        : handler(handler), registeredSignal(type)
{
    signals[registeredSignal] = &handler;

    /* Install signal handler. */
    struct sigaction action;
    action.sa_sigaction = globalHandler;
    action.sa_mask = 0;
    action.sa_flags = SA_SIGINFO;

    sigaction(registeredSignal, &action, &oldAction);
}

Signal::~Signal()
{
    /* Uninstall signal handler. */
    sigaction(registeredSignal, &oldAction, NULL);
    signals[registeredSignal] = 0;
}

}
