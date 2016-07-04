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

Object.defineProperty(Function.prototype, 'parameters', {
  enumerable: false,
  configurable: false,

  get: function () {
    var asString = this.toString();
    var match = asString.match(/^function[^(]+\(([^)]+)/)

    if (!match || !match[1]) {
      throw new SyntaxError('Could not parse function parameters');
    }

    var parameterList = match[1];
    return parameterList
      /* This is the worst regular expression. */
      .replace(/\/\*.*?\*\//, '') // Remove comments.
      .replace(/\s+/, '') // Remove whitespace.
      .split(/,/); // Split the identifiers!
  }
});
