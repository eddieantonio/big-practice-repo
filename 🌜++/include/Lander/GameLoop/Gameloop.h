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

#ifndef LANDER_GAMELOOP_GAMELOOP_H
#define LANDER_GAMELOOP_GAMELOOP_H

#include "Signal/Alarm.h"
#include "Lander/GameLoop/Definition.h"

namespace Lander {
namespace GameLoop {

/**
 * Creates and starts the gameloop.
 */
class GameLoop {
public:
    GameLoop(Definition& def);
    ~GameLoop();
    GameLoop& start();
    GameLoop& stop();

    FrameCounter &frameNo();
    FrameCounter doFrame();


protected:

    Definition& handler;
    volatile bool shouldContinue;
    FrameCounter elapsedFrames;
    Signal::Alarm alarm;
};

}
}
#endif /* LANDER_GAMELOOP_GAMELOOP_H */
