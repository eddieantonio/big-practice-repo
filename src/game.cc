#include <cstdlib>

#include "Sketch/Sketch.h"
#include "Lander/GameLoop/Definition.h"
#include "Lander/GameLoop/GameLoop.h"

namespace Lander {

class Game : public Lander::GameLoop::Definition {
public:
    Game(const Sketch::Sketch& sketchpad)
        : sketcher(sketchpad) { }

    void eachFrame(GameLoop::FrameCounter frame)
    {
        drawRandomLine();
    }

protected:
    const Sketch::Sketch &sketcher;

    void drawRandomLine() {
        sketcher
            .color(rand() % 0x1000000)
            .drawLine(
                    rand() % 640,
                    rand() % 480,
                    rand() % 640,
                    rand() % 480);
    }
};

void startGame(const Sketch::Sketch& sketcher) {
    Game game(sketcher);
    GameLoop::GameLoop loop(game);

    loop.start();
}

}
