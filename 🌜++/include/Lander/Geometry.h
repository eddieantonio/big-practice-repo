/*
 * Copyright (C) 2016 Eddie Antonio Santos <easantos@ualberta.ca>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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

    bool exists() const;

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
