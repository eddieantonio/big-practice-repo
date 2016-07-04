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
