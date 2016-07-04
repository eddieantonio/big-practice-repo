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
