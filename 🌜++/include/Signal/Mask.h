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

#ifndef SIGNAL_MASK_H
#define SIGNAL_MASK_H

#include <signal.h>

namespace Signal {

class Mask {
public:
    enum class Manner {
        Block = SIG_BLOCK,
        Unblock = SIG_UNBLOCK,
        Replace = SIG_SETMASK
    };

    Mask(Manner how, sigset_t set);
    ~Mask();

private:
    sigset_t oldSet;
};

}
#endif /* SIGNAL_MASK_H */
