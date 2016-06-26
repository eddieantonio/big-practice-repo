import test from 'ava';

import Option, {Some, None} from './';

test('new Some(value)', t => {
  const value = {};
  t.is(new Some(value).getOr({}), value);
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
    t.pass();
  }
});

test('Option#of(null) === None', t => {
  t.is(Option.of(null), None);
  t.false(Option.of(null).hasValue());
});

test('Option#of(undefined) === None', t => {
  t.is(Option.of(undefined), None);
  t.false(Option.of(undefined).hasValue());
});

test('Option#of(false) !== None', t => {
  t.true(Option.of(false).hasValue());
  t.false(Option.of(false).empty);
  t.is(Option.of(false).getOr({}), false);
});

test('Option#of(0) !== None', t => {
  t.true(Option.of(0).hasValue());
  t.false(Option.of(0).empty);
  t.is(Option.of(0).getOr({}), 0);
});

test('Option#of(NaN) !== None', t => {
  t.true(Option.of(NaN).hasValue());
  t.false(Option.of(NaN).empty);
  t.true(Number.isNaN(Option.of(NaN).getOr(0)));
});

test('Option#of("") !== None', t => {
  t.true(Option.of('').hasValue());
  t.false(Option.of(``).empty);
  t.is(Option.of("").getOr({}), "");
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

