#include "game.h"
#include "sketchpad.h"

int main(int argc, char** argv) {
    if (argc >= 2) {
        sketchpad_register(stdout);
    } else {
        sketchpad_register_default();
    }

    start_game();

    sketchpad_shutdown();
    return 0;
}
