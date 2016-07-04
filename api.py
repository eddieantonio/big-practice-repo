#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

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

import io
from subprocess import Popen, check_output, PIPE

import hug
from falcon import HTTP_400
from apertium_streamparser import parse as parse_stream, Knownness

FST_FILE = 'fst'
HFST_LOOKUP = check_output('which hfst-lookup', shell=True)\
    .decode('UTF-8').rstrip('\n')


class BadRequest(RuntimeError):
    def __init__(self, message=None, **kwargs):
        super(BadRequest, self).__init__(message)
        self._msg = message
        self._args = kwargs

    def as_dict(self):
        new_dict = {}
        if self._msg is not None:
            new_dict.update(message=self._msg)
        new_dict.update(self._args)


@hug.post('/')
def api(search: str, response):
    try:
        return lemma_from_json(search)
    except BadRequest as error:
        response.status = HTTP_400
        return error.as_dict()


def lemma_from_json(search):
    if len(search) == 0:
        raise BadRequest('Must send a search string')
    if len(search) > 1024:
        raise BadRequest('Search wordform is too long',
                         actual_length=len(search),
                         maximum_allowable=1024)

    return {
        "search": extract_lemma(search),
        "version": "1.0.2",
        "utf-8": "🆗"
    }


def extract_lemma(wordform):
    lines = hfst_lookup(wordform)
    return list(generate_lemmas(lines))


def hfst_lookup(wordform):
    fst_input = (wordform + '\n').encode('UTF-8')
    arguments = [HFST_LOOKUP, '-q', '--output-format', 'apertium', FST_FILE]
    options = dict(shell=False, stdin=PIPE, stdout=PIPE)

    with Popen(arguments, **options) as process:
        output, _ = process.communicate(fst_input)

    return output.decode('UTF-8')


def generate_lemmas(lines):
    """
    Yields lemmas from the given lines of output.
    """
    for line in lines:
        lexical_units = parse_stream(line)
        for lexical_unit in lexical_units:
            if lexical_unit.knownness != Knownness.known:
                continue

            for reading in lexical_unit.readings:
                yield reading[0].baseform
