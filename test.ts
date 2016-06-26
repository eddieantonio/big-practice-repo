import test from 'ava';

import Option, {Some, None} from './';

test('new Some(value)', t => {
  const value = {};
  t.is(new Some(value).get(), value);
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

test('Option#of(null) === None', t => {
  t.is(Option.of(null), None);
  // TODO: throw a specific error.
  t.throws(() => Option.of(null).get());
});

test('Option#of(undefined) === None', t => {
  t.is(Option.of(undefined), None);
  // TODO: throw a specific error.
  t.throws(() => Option.of(undefined).get());
});

test('Option#of(false) !== None', t => {
  t.true(Option.of(false).hasValue);
  t.false(Option.of(false).empty);
  t.notThrows(() => Option.of(false).get());
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

  t.notThrows(() => Option.of(1).flatmap(sqrt).get());
  t.is(Option.of(-1).flatmap(sqrt), None as Option<any>);
});
