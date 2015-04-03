import Immutable from 'immutable';

export default class NonGenericVars {
  constructor(types) {
    this.types = types || Immutable.Set();
  }

  extend(type) {
    return new NonGenericVars(this.types.add(type));
  }
}
