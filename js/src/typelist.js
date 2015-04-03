export default class TypeList {
  constructor() {
    let args = Array.slice(arguments);

    if (arguments.length === 1 && arguments[0].constructor === Array) {
      this.types = arguments[0];
    } else {
      this.types = Array.slice(arguments);
    }
    if (this.types.some( (t) => t.class !== 'Type' )) {
      throw new Error();
    }
    this.class = "TypeList";
  }

  toString(separator) {
    return this.types.map((type) => type.toString()).join(separator);
  }

  map(f) {
    return new TypeList(this.types.map(f));
  }
}
