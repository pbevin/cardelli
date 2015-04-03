
export default class Symbol {
  constructor(name) {
    this.name = name;
    this.class = "Symbol";
  }

  toString() {
    return this.name;
  }

  equals(other) {
    return this.name === other.name;
  }
}
