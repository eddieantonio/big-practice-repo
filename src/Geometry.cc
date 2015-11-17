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

/*
 * Point utilities!
 */

bool Point::exists() const
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
    if (isZeroLength() || other.isZeroLength())
        return INVALID_POINT;

    // SKIPPED: Fail if the segments share an end-point.

    //  (1) Translate the system so that point A is on the origin.
    Line ab = this->translate(-start);
    Line cd = other.translate(-start);

    double &bᵪ = ab.end.x, &bᵧ = ab.end.y,
           &cᵪ = cd.start.x, &cᵧ = cd.start.y,
           &dᵪ = cd.end.x, &dᵧ = cd.end.y;

    // Discover the length of segment AB.
    auto distanceAB = sqrt(bᵪ * bᵪ + bᵧ * bᵧ);

    //  (2) Rotate the system so that point B is on the positive X axis.
    auto theCos = bᵪ / distanceAB;
    auto theSin = bᵧ / distanceAB;
    auto newCX = cᵪ * theCos + cᵧ * theSin;
    cᵧ = cᵧ * theCos - cᵪ * theSin;
    cᵪ = newCX;
    auto newDX = dᵪ * theCos + dᵧ * theSin;
    dᵧ = dᵧ * theCos - dᵪ * theSin;
    dᵪ = newDX;

    // Fail if segment CD doesn't cross line AB.
    if (cd.below(0.0) || cd.above(0.0))
        return INVALID_POINT;

    // (3) Discover the position of the intersection point along line AB.
    auto ABpos = dᵪ + (cᵪ - dᵪ) * dᵧ / (dᵧ - cᵧ);

    // Fail if segment CD crosses line AB outside of segment AB.
    if ((ABpos < 0.0) || (ABpos > distanceAB))
        return INVALID_POINT;

    // (4) Apply the discovered position to line AB in the original coordinate system.
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
