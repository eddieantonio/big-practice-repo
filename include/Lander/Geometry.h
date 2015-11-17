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

    /**
     * @return Point the mirror copy of this point flipped about y=-x.
     */
    Point operator -() const;

    bool operator==(const Point&) const;

    /**
     * @return Point pair-wise addition of the points.
     */
    Point operator+(const Point& other) const;
    /**
     * @return Point pair-wise subtraction of the points.
     */
    Point operator-(const Point& other) const;
};

/**
 * A 2D line.
 */
struct Line {
    Line() : start(Point()), end(Point()) { }
    Line(Point a, Point b) : start(a), end(b) { }
    Point start, end;

    Point intersection(const Line& other) const;

    /* Lots of utility functions! */

    bool isZeroLength() const;

    bool below(double y) const;
    bool above(double y) const;

    Line translate(const Point& origin) const;
};

}

#endif /* LANDER_GEOMETRY_H */
