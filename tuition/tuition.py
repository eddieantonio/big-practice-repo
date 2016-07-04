#!/usr/bin/env python

# Copyright (C) 2016 Eddie Antonio Santos <easantos@ualberta.ca>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

