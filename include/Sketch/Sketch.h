#ifndef SKETCH_SKETCH_H
#define SKETCH_SKETCH_H

#include <cstdint>
#include <ostream>

namespace Sketch {

typedef int_least32_t RGB;

/**
 * Handles a Sketchpad instance.
 */
class Sketch {
public:
    Sketch(std::ostream& process);

    const Sketch& drawLine(double x1, double y1, double x2, double y2) const;
    const Sketch& eraseLine(double x1, double y1, double x2, double y2) const;
    const Sketch& color(RGB triple) const;
    const Sketch& pause(double time) const;
    const Sketch& end() const;

private:
    std::ostream& process;
};

}

#endif /* SKETCH_SKETCH_H */
