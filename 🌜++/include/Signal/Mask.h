#ifndef SIGNAL_MASK_H
#define SIGNAL_MASK_H

#include <signal.h>

namespace Signal {

class Mask {
public:
    enum class Manner {
        Block = SIG_BLOCK,
        Unblock = SIG_UNBLOCK,
        Replace = SIG_SETMASK
    };

    Mask(Manner how, sigset_t set);
    ~Mask();

private:
    sigset_t oldSet;
};

}
#endif /* SIGNAL_MASK_H */
