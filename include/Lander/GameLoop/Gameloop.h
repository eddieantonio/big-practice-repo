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
