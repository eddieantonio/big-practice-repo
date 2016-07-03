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

/**
 * There should be a strong assertion that int is a small integer (31-bit
 * unsigned in Google V8).
 */
type int = number;

export default class LinearFeedbackShiftRegister {
  protected _seed: int;

  constructor(seed: number) {
    this.seed = 0;
  }

  next(): int {
    const original = this.seed;
    // Taps for a 30 bit LSRF from Table 3 in:
    // http://www.xilinx.com/support/documentation/application_notes/xapp052.pdf
    const output = xnor(tap(original, 1),
                        xnor(tap(original, 4),
                             xnor(tap(original, 6),
                                  tap(original, 30))));
    return this.seed = (original >>> 1) | (output << 30);
  }

  /**
   * Returns the seed.
   */
  get seed(): int {
    return this._seed;
  }

  /**
   * Ensures that the seed is set to an acceptable integer.
   */
  set seed(val: number) {
    this._seed = toSmallInt31(val);
  }
}

const defaultGenerator = new LinearFeedbackShiftRegister(Math.random() * 0xffffffff);

export function random() {
  return defaultGenerator.next();
}

function toSmallInt31(num: number): int {
  return num & 0x7fffffff;
}

function xnor(a: int, b: int): int {
  return ~(a ^ b);
}

/* Take the bit number and return 0 or 1. */
function tap(num: int, bit: int): int {
  const mask = 0x01 << (30 - bit);
  /* This crazy coercion turns the number into a boolean, then the boolean
   * into an int. */
  return +!!(num & mask);
}
