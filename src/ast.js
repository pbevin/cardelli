import _ from 'lodash';

// Simple metaprogramming: ast(name, params)
// defines a class that visits with the given
// name, and passes the given params into the
// visitor.
function ast(name, argNames) {
  return function() {
    let args = Array.slice(arguments);

    let constructor = function() {
      let obj = _.zipObject(argNames, args);
      for (let k in obj) {
        if (obj[k].class != 'Symbol' && obj[k].class != 'ASTNode') {
          console.log("Can't add this to an ASTNode: ", obj[k]);
          throw new Error();
        }
        this[k] = obj[k];
      }
      this.class = 'ASTNode';
    }

    constructor.prototype.visit = function(v) {
      let result = v[name].apply(v, args);
      return result;
    };

    constructor.prototype.toString = function() {
      let content = args.map((a) => a.toString()).join(" ");
      return `(${name} ${content})`;
    }

    return new constructor();
  }
}

export default {
  Ide: ast("ide", ["ide"]),
  Cond: ast("cond", ["test", "ifTrue", "ifFalse"]),
  Lambda: ast("lambda", ["binder", "body"]),
  Apply: ast("apply", ["fun", "arg"]),
  Block: ast("block", ["decl", "scope"]),

  Def: ast("def", ["binder", "def"]),
  Seq: ast("seq", ["first", "second"]),
  Rec: ast("rec", ["rec"]),
};
