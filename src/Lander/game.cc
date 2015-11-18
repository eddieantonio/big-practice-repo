#include <cstdlib>
#include <iostream>

#include "Sketch/Sketch.h"
#include "Signal/Signal.h"

#include "Lander/GameLoop/Definition.h"
#include "Lander/GameLoop/GameLoop.h"
#include "Lander/Geometry.h"

namespace {

const Sketch::RGB RED = 0xFF0000;
const Sketch::RGB ACCENT_1 = 0xAABA62;
const Sketch::RGB ACCENT_2 = 0x70588C;

const double CENTER = 480 / 2;
const double DISTANCE_APART = 10;
const double TOP = CENTER - DISTANCE_APART / 2;
const double BOTTOM = CENTER + DISTANCE_APART /2;

using Lander::GameLoop::GameLoop;

/**
 * A lot of pomp & circumstance just to stop the gameloop.
 */
class SigIntHandler : public Signal::SignalAction {
    GameLoop& gameLoop;
public:
    SigIntHandler(GameLoop& loop) : gameLoop(loop) { }
    virtual void handle(int signal)
    {
        gameLoop.stop();
    }
};

const Sketch::Sketch& operator <<(const Sketch::Sketch& sketcher, Sketch::RGB color)
{
    sketcher.color(color);
    return sketcher;
}

const Sketch::Sketch& operator <<(const Sketch::Sketch& sketcher, const Lander::Line& l)
{
    sketcher.drawLine(l.start.x, l.start.y, l.end.x, l.end.y);
    return sketcher;
}

const Sketch::Sketch& operator >>(const Sketch::Sketch& sketcher, const Lander::Line& l)
{
    sketcher.eraseLine(l.start.x, l.start.y, l.end.x, l.end.y);
    return sketcher;
}

}

namespace Lander {


class Game : public Lander::GameLoop::Definition {
public:
    Game(const Sketch::Sketch& sketchpad)
        : sketcher(sketchpad),
          topLine(Point(10, TOP), Point(630, TOP)),
          bottomLine(Point(10, BOTTOM), Point(630, BOTTOM))
    { }

    void eachFrame(GameLoop::GameLoop &loop)
    {
        this->loop = &loop;

        sketcher >> topLine >> bottomLine;

        topLine.start.y += 1.0/60.0;
        bottomLine.start.y -= 1.0/60.0;

        sketcher
            << ACCENT_1 << topLine
            << ACCENT_2 << bottomLine;

        if (topLine.end.y > bottomLine.start.y) {
            stop(loop);
        }

        if (topLine.intersection(bottomLine).exists()) {
            sketcher << RED << topLine << bottomLine;
            stop(loop);
        }
    }

protected:
    const Sketch::Sketch &sketcher;
    GameLoop::GameLoop *loop;
    Line topLine, bottomLine;

    void stop(GameLoop::GameLoop& loop)
    {
        loop.stop();
        sketcher.pause(5);
    }
};

void startGame(const Sketch::Sketch& sketcher) {
    Game game(sketcher);
    GameLoop::GameLoop loop(game);
    SigIntHandler stopOnSignal(loop);
    Signal::Signal sigint(SIGINT, stopOnSignal);

    loop.start();
    sketcher.end();
}

}
