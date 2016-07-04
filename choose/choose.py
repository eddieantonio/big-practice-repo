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

import os
import sys
import random

USAGE_TEXT = """
Usage:
    {0} args...

Where args are completely arbitrary arguments.
""".format(sys.argv[0])

def invalid_usage(msg):
    sys.stderr.write(msg)
    sys.stderr.write(USAGE_TEXT)
    exit(-1)

if __name__ == '__main__':
    choices = sys.argv[1:]
    if not choices:
        invalid_usage('Need at least one arugment!')
    print(random.choice(choices))
