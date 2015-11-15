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
        return c;
    }
};

}
}

#endif /* LANDER_UTIL_COutputBuffer */
