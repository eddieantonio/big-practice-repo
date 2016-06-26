abstract class Option<T> {
  public empty: boolean;

  abstract hasValue(): this is Some<T>;

  abstract getOr<U>(alternative: U): T | U;
  abstract getOrElse<U>(otherwise: () => U): T | U;

  abstract map<U>(f: (v: T) => U): Option<U>;
  abstract flatmap<U>(f: (v: T) => Option<U>): Option<U>;

  static of<T>(value: T): Option<T> {
    if (value === null || value === undefined) {
      return None as Option<any>;
    } else {
      return new Some(value);
    }
  }
}
export default Option;

export class Some<T> implements Option<T> {
  private _value: T;

  constructor(value: T) {
    this._value = value;
  }

  /**
   * Get the underlying JavaScript value. Note that methods Option<T>#getOr()
   * and Option<T>#getOrElse() are prefered over directly accesing
   * Some<T>#value.
   */
  get value(): T {
    return this._value;
  }

  hasValue() {
    return true;
  }

  get empty() {
    return false;
  }

  getOr<U>(_unused: U): T {
    return this.value;
  }

  getOrElse<U>(_unused: () => U): T {
    return this.value;
  }

  map<U>(f: (v: T) => U) {
    return new Some(f(this.value));
  }

  flatmap<U>(f: (v: T) => Option<U>): Option<U> {
    return f(this.value);
  }
}

const None = new (class None<T> implements Option<T> {
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

  getOr<U>(value: U): U {
    return value;
  }

  getOrElse<U>(func: () => U): U {
    return func();
  }

  map<U>(f: (v: T) => U): Option<U> {
    return this as Option<any>;
  }

  flatmap<U>(f: (v: T) => Option<U>): Option<U> {
    return this as Option<any>;
  }

  /**
   * Returns None as an Option of the given type.
   */
  as<T>(): Option<T> {
    return this as Option<any>;
  }
});
export { None };

/**
 * A type with no values.
 */
class Invalid {
}
