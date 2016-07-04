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

#include <iostream>
#include <exception>

#define _BSD_SOURCE
#include <cstdio>

#include "Sketch/Sketch.h"
#include "Lander/Util/COutputBuffer.h"

namespace Lander {
void startGame(const Sketch::Sketch& sketcher);
}

static void do_it(const Sketch::Sketch& sketchpad) {
    Lander::startGame(sketchpad);
}

static int to_sketchpad() {
    FILE* process = popen("java -jar Sketchpad.jar", "w");
    if (process == NULL) {
        throw std::exception();
    }

    Lander::Util::COutputBuffer buffer(process);
    std::ostream output(&buffer);

    Sketch::Sketch sketchpad(output);
    do_it(sketchpad);

    return pclose(process);
}

static int to_terminal() {
    Sketch::Sketch sketchpad((std::cout));
    do_it(sketchpad);

    return 0;
}

int main(int argc, char* argv[]) {
    return (argc >= 2)
        ? to_terminal()
        : to_sketchpad();
}
