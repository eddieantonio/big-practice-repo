#ifndef LANDER_GEOMETRY_H
#define LANDER_GEOMETRY_H

namespace Lander {

/**
 * A 2D point.
 */
struct Point {
    Point() : x(0.0), y(0.0) { }
    Point(double x, double y) : x(x), y(y) { }
    double x, y;

    /**
     * @return whether the point is valid.
     */
    operator bool() const;
};

/**
 * A 2D line.
 */
struct Line {
    Line() : start(Point()), end(Point()) { }
    Line(Point a, Point b) : start(a), end(b) { }
    Point start, end;

    Point intersection(Line other) const;
};

}

#endif /* LANDER_GEOMETRY_H */
