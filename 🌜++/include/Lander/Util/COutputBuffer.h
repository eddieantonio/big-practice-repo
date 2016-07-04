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

#ifndef LANDER_UTIL_COutputBuffer
#define LANDER_UTIL_COutputBuffer

#include <ostream>

namespace Lander {
namespace Util {

/**
 * Wraps a C stream as an output buffer.
 */
class COutputBuffer : public std::streambuf {
public:
    COutputBuffer(FILE * const fp) : c_stream(fp) { }

protected:
    FILE * const c_stream;

    virtual int_type overflow(int_type c)
    {
        if (c != EOF) {
            if (fputc(c, c_stream) == EOF) {
                return EOF;
            }
        }

        if (c == '\n') {
            fflush(c_stream);
        }

        return c;
    }
};

}
}

#endif /* LANDER_UTIL_COutputBuffer */
