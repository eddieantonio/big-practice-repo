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

abstract class Option<A> {
  public empty: boolean;

  abstract hasValue(): this is Some<A>;

  abstract getOr<B>(alternative: B): A | B;
  abstract getOrElse<B>(otherwise: () => B): A | B;

  abstract map<B>(f: (v: A) => B): Option<B>;
  abstract flatmap<B>(f: (v: A) => Option<B>): Option<B>;

  /**
   * Wraps a value in an Option burrito.
   */
  static return<A>(value: A): Some<A> {
    return new Some(value);
  }

  /**
   * Like return but converts `null` and `undefined` into None.
   */
  static of<A>(value: A): Option<A> {
    if (value === null || value === undefined) {
      return None as Option<any>;
    } else {
      return Option.return(value);
    }
  }

  /**
   * Get rid of NaNs by using this static method!
   *
   * Option.number(parseInt('hello', 10))
   *    .getOrElse(() => throw new Error('Invalid number'));
   */
  static number(value: number): Option<number> {
    if (Number.isNaN(value)) {
      return None as Option<number>;
    } else {
      return Option.return(value);
    }
  }
}
export default Option;

export class Some<A> implements Option<A> {
  private _value: A;

  constructor(value: A) {
    this._value = value;
  }

  /**
   * Get the underlying JavaScript value. Note that methods Option<T>#getOr()
   * and Option<T>#getOrElse() are prefered over directly accesing
   * Some<T>#value.
   */
  get value(): A {
    return this._value;
  }

  hasValue() {
    return true;
  }

  get empty() {
    return false;
  }

  getOr<B>(_unused: B): A {
    return this.value;
  }

  getOrElse<B>(_unused: () => B): A {
    return this.value;
  }

  map<B>(f: (v: A) => B): Some<B> {
    return Option.return(f(this.value));
  }

  flatmap<B>(f: (v: A) => Option<B>): Option<B> {
    return f(this.value);
  }
}

const None = new (class None<A> implements Option<A> {
  get empty() {
    return true;
  }

  /**
   * This is **always** an invalid operation. Note that the return type
   * Invalid has no values, and is not assignable to any type. The "true"
   * return type is never. However, the bottom/nothing/never is assignable to
   * any value, hence Invalid prevents TypeScript from allowing trivial
   * assignments from Invalid to something.
   */
  get value(): Invalid {
    throw new ReferenceError('Cannot get value from None.');
  }

  hasValue() {
    return false;
  }

  getOr<B>(value: B): B {
    return value;
  }

  getOrElse<B>(func: () => B): B {
    return func();
  }

  map<B>(f: (v: A) => B): None<B> {
    return this as None<any>;
  }

  flatmap<B>(f: (v: A) => Option<B>): None<B> {
    return this as None<any>;
  }

  /**
   * Returns None as an Option of the given type.
   */
  as<A>(): Option<A> {
    return this as Option<any>;
  }
});
export { None };

/**
 * A type with no values.
 */
class Invalid {
}
