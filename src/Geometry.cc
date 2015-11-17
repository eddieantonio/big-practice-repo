#include <cmath>
#include <limits>

#include "Lander/Geometry.h"

namespace {

const Lander::Point INVALID_POINT(
        std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN()
);

}

namespace Lander {

/* Point utilities! */
Point::operator bool() const
{
    return !(isnan(x) || isnan(y));
}

Point Point::operator-() const
{
    return Point(-x, -y);
}

Point Point::operator+(const Point& other) const
{
    return Point(x + other.x, y + other.y);
}

Point Point::operator-(const Point& other) const
{
    return *this + -other;
}

bool Point::operator==(const Point& other) const
{
    return x == other.x && y == other.y;
}

/*
 * Line utilities!
 */

Line Line::translate(const Point& origin) const
{
    return Line(start + origin, end + origin);
}

/**
 * Based on a:
 * public domain function by Darel Rex Finley, 2006
 * Determines the intersection point of the line segment defined by points A and B
 * with the line segment defined by points C and D.
 *
 * @return a Point if the intersection point was found.
 * Returns an invalid Point if there is no determinable intersection point.
 */
Point Line::intersection(const Line& other) const
{
    // Fail if either line segment is zero-length.
    if (isZeroLength() || other.isZeroLength()) return INVALID_POINT;

    // SKIPPED: Fail if the segments share an end-point.

    //  (1) Translate the system so that point A is on the origin.
    Line ab = this->translate(-start);
    Line cd = other.translate(-start);

    // Discover the length of segment AB.
    auto distanceAB = sqrt(ab.end.x * ab.end.x + ab.end.y * ab.end.y);

    //  (2) Rotate the system so that point B is on the positive X axis.
    auto theCos = ab.end.x / distanceAB;
    auto theSin = ab.end.y / distanceAB;
    auto newCX = cd.start.x * theCos + cd.start.y * theSin;
    cd.start.y = cd.start.y * theCos - cd.start.x * theSin;
    cd.start.x = newCX;
    auto newDX = cd.end.x * theCos + cd.end.y * theSin;
    cd.end.y = cd.end.y * theCos - cd.end.x * theSin;
    cd.end.x = newDX;

    // Fail if segment CD doesn't cross line AB.
    if (cd.below(0.0) || cd.above(0.0))
        return INVALID_POINT;

    // (3) Discover the position of the intersection point along line A-B.
    auto ABpos = cd.end.x + (cd.start.x - cd.end.x) * cd.end.y / (cd.end.y - cd.start.y);

    // Fail if segment C-D crosses line A-B outside of segment A-B.
    if ((ABpos < 0.0) || (ABpos > distanceAB))
        return INVALID_POINT;

    //  (4) Apply the discovered position to line A-B in the original coordinate system.
    return Point(
        start.x + ABpos * theCos,
        start.y + ABpos * theSin
    );
}

bool Line::isZeroLength() const
{
    return start == end;
}


bool Line::below(double y) const
{
    return start.y < y && end.y < y;
}

bool Line::above(double y) const
{
    return start.y > y && end.y > y;
}

}
