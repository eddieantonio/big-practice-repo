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

import test from 'ava';

import Option, {Some, None} from './';


test('new Some(value)', t => {
  const value = {};
  t.is(new Some(value).getOr({}), value);
});

test('Option.return(truthy)', t => {
  const ref = {};
  const sym = Symbol('arbitrary');
  t.is(Option.return(1).value, 1);
  t.is(Option.return(true).value, true);
  t.is(Option.return('💩').value, '💩');
  t.is(Option.return(sym).value, sym);
});

test('Option.return(null)', t => {
  /* Tests that return ALWAYS wraps a given value. */
  t.is(Option.return(null).value, null);
});

test('Some#getOr()', t => {
  const value = {};
  t.is(new Some(value).getOr({}), value);
});


test('None#as<T>()', t => {
  /* Let it be some random type. */
  let option: Option<Date>;
  t.is(option = None.as<Date>(), None as Option<any>);
});

test('Option#hasValue()', t => {
  /* Checks for TypeScript type assertions. */
  let some: Option<boolean> = new Some(true);
  let none: Option<boolean> = None.as<boolean>();

  t.plan(2);

  if (some.hasValue()) {
    /* TypeScript test: some.value should be unavailable if some is determined
     * to be Option<T> or None<T> by the type checker.*/
    t.is(some.value, true);
  } else {
    t.fail('Some#hasValue() must never be false.');
  }

  if (none.hasValue()) {
    t.fail('None#hasValue() must never be true.');
  } else {
    t.throws(
      () => (<any> none).value,
      ReferenceError,
      'None#value must throw a JavaScript error.'
    );
  }
});

test('Option.of(null) === None', t => {
  t.is(Option.of(null), None);
  t.false(Option.of(null).hasValue());
});

test('Option.of(undefined) === None', t => {
  t.is(Option.of(undefined), None);
  t.false(Option.of(undefined).hasValue());
});

test('Option.of(false) !== None', t => {
  t.true(Option.of(false).hasValue());
  t.false(Option.of(false).empty);
  t.is(Option.of(false).getOr({}), false);
});

test('Option.of(0) !== None', t => {
  t.true(Option.of(0).hasValue());
  t.false(Option.of(0).empty);
  t.is(Option.of(0).getOr({}), 0);
});

test('Option.of(NaN) !== None', t => {
  t.true(Option.of(NaN).hasValue());
  t.false(Option.of(NaN).empty);
  t.true(Number.isNaN(Option.of(NaN).getOr(0)));
});

test('Option.of("") !== None', t => {
  t.true(Option.of('').hasValue());
  t.false(Option.of(``).empty);
  t.is(Option.of("").getOr({}), "");
});

test('Option.number(finite)', t => {
  t.is(Option.number(-1e100).getOr(NaN), -1e100);
});

test('Option.number(±∞)', t => {
  t.is(Option.number(+Infinity).getOr(NaN), +Infinity);
  t.is(Option.number(-Infinity).getOr(NaN), -Infinity);
});

test('Option.number(NaN)', t => {
  t.is(Option.number(NaN), None as Option<number>);
});

test('Option#map()', t => {
  const truthy = (x: any) => !!x;

  t.is(None.map(truthy), None as Option<any>);
  t.deepEqual(Option.of(0).map(truthy), Option.of(false));
});

test('Option#flatmap()', t => {
  function sqrt(n: number) {
    if (n < 0) {
      return None.as<number>();
    } else {
      return Option.of(Math.sqrt(n));
    }
  }

  t.is(Option.of(1).flatmap(sqrt).getOr(NaN), 1);
  t.is(Option.of(-1).flatmap(sqrt), None as Option<any>);
});
