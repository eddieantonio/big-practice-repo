#include <cmath>
#include <limits>

#include "Lander/Geometry.h"

//#include <iostream>

namespace {

const Lander::Point INVALID_POINT(
        std::numeric_limits<double>::quiet_NaN(),
        std::numeric_limits<double>::quiet_NaN()
);

/**
 * Based on a:
 * public domain function by Darel Rex Finley, 2006
 * Determines the intersection point of the line segment defined by points A and B
 * with the line segment defined by points C and D.
 *
 * @return a Point if the intersection point was found.
 * Returns an invalid Point if there is no determinable intersection point.
 */
bool lineSegmentIntersection(
    double Ax, double Ay,
    double Bx, double By,
    double Cx, double Cy,
    double Dx, double Dy,
    double *X, double *Y
) {
    double  distAB, theCos, theSin, newX, ABpos ;

    //  Discover the length of segment A-B.
    distAB = sqrt(Bx * Bx + By * By);

    //std::cerr << Ax << ',' << Ay << "=>" << Bx << ',' << By << std::endl;
    //std::cerr << Cx << ',' << Cy << "=>" << Dx << ',' << Dy << std::endl;

    //  (2) Rotate the system so that point B is on the positive X axis.
    theCos = Bx / distAB;
    theSin = By / distAB;
    newX = Cx * theCos + Cy * theSin;
    Cy = Cy * theCos - Cx * theSin;
    Cx = newX;
    newX = Dx * theCos + Dy * theSin;
    Dy = Dy * theCos - Dx * theSin;
    Dx = newX;

    //  Fail if segment C-D doesn't cross line A-B.
    if ((Cy < 0.0 && Dy < 0.0) || (Cy >= 0.0 && Dy >= 0.0))
        return false;

    //  (3) Discover the position of the intersection point along line A-B.
    ABpos=Dx+(Cx-Dx)*Dy/(Dy-Cy);

    //  Fail if segment C-D crosses line A-B outside of segment A-B.
    if ((ABpos < 0.0) || (ABpos > distAB)) return false;

    //  (4) Apply the discovered position to line A-B in the original coordinate system.
    *X=Ax+ABpos*theCos;
    *Y=Ay+ABpos*theSin;

    //  Success.
    return true;
}

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

Point Line::intersection(const Line& other) const
{
    double x, y;

    // Fail if either line segment is zero-length.
    if (isZeroLength() || other.isZeroLength()) return INVALID_POINT;

    // SKIPPED: Fail if the segments share an end-point.

    //  (1) Translate the system so that point A is on the origin.
    Line a = this->translate(-start);
    Line b = other.translate(-start);

    bool didIntersect = lineSegmentIntersection(
            a.start.x, a.start.y,
            a.end.x, a.end.y,
            b.start.x, b.start.y,
            b.end.x, b.end.y,
            &x, &y
    );

    if (didIntersect) {
        return Point(x, y);
    } else {
        return INVALID_POINT;
    }
}

bool Line::isZeroLength() const
{
    return start == end;
}

}
