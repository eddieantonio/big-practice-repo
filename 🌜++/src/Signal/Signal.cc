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

#include "Signal/Signal.h"

#include <signal.h>

namespace {

/**
 * An entry for each signal.
 */
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
    sigaction(registeredSignal, &oldAction, nullptr);
    signals[registeredSignal] = nullptr;
}

}
