#include <iostream>
#include <exception>

#define _BSD_SOURCE
#include <cstdio>

#include "Sketch/Sketch.h"
#include "Lander/Util/COutputBuffer.h"

static void do_it(const Sketch::Sketch& sketchpad) {
    sketchpad
        .color(0xFF00FF)
        .drawLine(10, 10, 100, 100)
        .pause(5)
        .end();
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
