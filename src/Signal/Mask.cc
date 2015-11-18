#include "Signal/Mask.h"

namespace Signal {

Mask::Mask(Manner how, sigset_t set)
{
    sigprocmask(static_cast<int>(how), &set, &oldSet);
}

Mask::~Mask()
{
    sigprocmask(SIG_SETMASK, &oldSet, 0);
}

}
