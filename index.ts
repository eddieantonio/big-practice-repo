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

  get value() {
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
