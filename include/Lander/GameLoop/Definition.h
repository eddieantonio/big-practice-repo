#ifndef LANDER_GAMELOOP_DEFINITION_H
#define LANDER_GAMELOOP_DEFINITION_H

namespace Lander {
namespace GameLoop {

typedef unsigned long FrameCounter;

/**
 * Creates the defintion of a frame function.
 */
class Definition {
public:
    virtual ~Definition() {};
    virtual void eachFrame(FrameCounter frame) = 0;
};

}
}

#endif /* LANDER_GAMELOOP_DEFINITION_H */
