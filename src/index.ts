abstract class Option<T> {
  public empty: boolean;
  public hasValue: boolean;

  abstract get(): T;
  abstract getOr<U>(alternative: U): T | U;
  abstract getOrElse<U>(otherwise: () => U): U;

  abstract map<U>(f: (v: T) => U): Option<U>;

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
  constructor(private value: T) {
  }

  get hasValue() {
    return true;
  }

  get empty() {
    return false;
  }

  get() {
    return this.value;
  }

  getOr() {
    return this.value;
  }

  getOrElse() {
    return this.value;
  }

  map<U>(f: (v: T) => U) {
    return new Some(f(this.value));
  }
}

const None = new (class None<T> implements Option<T> {
  get empty() {
    return true;
  }

  get hasValue() {
    return false;
  }

  get(): T {
    throw new Error();
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
});
export { None };
