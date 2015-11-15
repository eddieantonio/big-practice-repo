#include <cmath>
#include <cstdio>
#include <ostream>

#include "Sketch/Sketch.h"

namespace Sketch {

Sketch::Sketch(std::ostream &stream)
    : process(stream) { }

const Sketch& Sketch::drawLine(double x1, double y1, double x2, double y2) const
{
    process
        << "drawSegment "
        << lround(x1) << ' '
        << lround(y1) << ' '
        << lround(x2) << ' '
        << lround(y2) << ' '
        << std::endl;

    return *this;
}

const Sketch& Sketch::eraseLine(double x1, double y1, double x2, double y2) const
{
    process
        << "eraseLine "
        << lround(x1) << ' '
        << lround(y1) << ' '
        << lround(x2) << ' '
        << lround(y2) << ' '
        << std::endl;

    return *this;
}

const Sketch& Sketch::color(RGB triple) const
{
    auto red   = (triple & 0xFF0000) >> 16;
    auto green = (triple & 0x00FF00) >> 8;
    auto blue  = (triple & 0x0000FF);

    process << "setColor "
        << red << ' '
        << green << ' '
        << blue << std::endl;

    return *this;
}

const Sketch& Sketch::pause(double time) const
{
    process << "pause " << time << std::endl;

    return *this;
}

const Sketch& Sketch::end() const
{
    process << "end" << std::endl;

    return *this;
}

}
