#!/usr/bin/env python

import decimal
import math

D = decimal.Decimal

MAX_AMOUNT = D('999.99')

message = """
A tuition of [1;31m${tuition:.2f}[m
will take [1;32m{days:n}[m days to pay off
@ [1;35m${amount:.2f}[m per day.
""".strip()

if __name__ == '__main__':
    import sys

    decimal.getcontext().rounding = decimal.ROUND_UP

    tuition = D(sys.argv[1])
    days = D(math.ceil(tuition / MAX_AMOUNT))

    amount = tuition / days

    print message.format(**vars())

