import Symbol from './symbol';
import TypeList from './typelist';

class Type {
  constructor() {
    this.class = "Type";
  }

  // PROCEDURE Prune(typeExp: TypeExp): TypeExp;
  //   BEGIN
  //     CASE typeExp^.class OF
  //     | VarType:
  //         IF typeExp^.instance = NIL THEN
  //           RETURN typeExp;
  //         ELSE
  //           typeExp^.instance := Prune(typeExp^.instance);
  //           RETURN typeExp^.instance;
  //         END;
  //     | OperType: RETURN typeExp;
  //     END;
  // END Prune;
  prune() {
    var type = this;
    return type.visit({
      varType() {
        if (!type.instance) {
          return type;
        } else {
          type.instance = type.instance.prune(); // mutable type
          return type.instance;
        }
      },
      operType() { return type; }
    });
  }

  // PROCEDURE OccursInType(typeVar: TypeExp; typeExp: TypeExp): BOOLEAN;
  //   BEGIN
  //     typeExp := Prune(typeExp);
  //     CASE typeExp^.class OF
  //     | VarType: RETURN SameType(typeVar, typeExp);
  //     | OperType: RETURN OccursInTypeList(typeVar, typeExp^.args);
  //     END;
  //   END OccursInType;
  occursInType(type2) {
    let type1 = this;
    type2 = type2.prune();
    return type1.visit({
      varType() { return type1.sameType(type2); },
      operType() { return type1.occursInTypeList(type2.args); }
    });
  }

  // PROCEDURE SameType(typeExp1, typeExp2: TypeExp): BOOLEAN;
  //   BEGIN RETURN typeExp1 = typeExp2; END SameType;
  sameType(type2) {
    return this === type2;
  }

  // PROCEDURE OccursInTypeList(typeVar: TypeExp; list: TypeList): BOOLEAN;
  //   BEGIN

  //     IF list = NIL THEN RETURN FALSE END;
  //     IF OccursInType(typeVar, list^.head) THEN RETURN TRUE END;
  //     RETURN OccursInTypeList(typeVar, list^.tail);
  //   END OccursInTypeList;
  occursInTypeList(typeList) {
    return typeList.types.some( (t) => this.occursInType(t) );
  }

  // PROCEDURE IsGeneric(typeVar: TypeExp; list: NonGenericVars): BOOLEAN;
  //   BEGIN RETURN NOT TypeMod.OccursInTypeList(typeVar, list); END IsGeneric;
  isGeneric(list) {
    !this.occursInTypeList(list);
  }
}

let names = "abcdefghijklmnopqrstuvwxyz".split('');
class TypeVar extends Type {
  constructor() {
    super();
    this.instance = null;
    this.varname = names.shift();
  }

  toString() {
    if (this.instance)
      return `${this.varname} = ${this.instance.toString()}`
    return this.varname;
  }

  visit(visitor) {
    return visitor.varType();
  }
}

class TypeOper extends Type {
  constructor(ide, args) {
    super();
    if (ide.class != 'Symbol') throw new Error();
    if (args.class != 'TypeList') throw new Error();

    this.ide = ide;
    this.args = args;
  }

  toString() {
    if (this.args.types.length == 0)
      return this.ide.name;
    else {
      return this.args.toString(this.ide.name);
    }
  }

  visit(visitor) {
    return visitor.operType();
  }
}


var Types = {
  Var(ide) {
    return new TypeVar();
  },

  Oper(ide, args) {
    if (!ide) throw new Error("null");
    if (!args) args = new TypeList();
    return new TypeOper(ide, args);
  },

  create(concreteType) {
    return Types.Oper(new Symbol(concreteType), new TypeList());
  },

  arrow(_types) {
    let types = Array.slice(arguments).map((t) => {
      if (typeof(t) === 'string') {
        return Types.create(t);
      } else {
        return t;
      }
    });

    return Types.Oper(new Symbol('->'), new TypeList(types));
  }
}

export default Types;
