import Immutable from 'immutable';
import Type from './type';
import Symbol from './symbol';

export default class Env {
  constructor(map) {
    if (!map) map = Immutable.Map();
    this.map = map;
  }

  extend(ident, type) {
    if (ident.class !== 'Symbol') throw new Error(ident.class);
    return new Env(this.map.set(ident.name, type));
  }

  preload(types) {
    let env = this;
    for (let k in types) {
      env = env.extend(new Symbol(k), types[k]);
    }
    return env;
  }

  // PROCEDURE Retrieve(ide: Ide; env: Env; list: NonGenericVars): TypeExp;
  //   BEGIN
  //     IF env = EnvMod.Empty THEN
  //       ErrorMod.Msg("Unbound ide");
  //       RETURN NIL;
  //     ELSIF SymbolMod.Equal(ide, env^.ide) THEN
  //       RETURN GenericVarMod.FreshType(env^.typeExp, list);
  //     ELSE
  //       RETURN Retrieve(ide, env^.tail, list);
  //     END;
  //   END Retrieve;
  retrieve(ident, nonGenericVars) {
    let type = this.map.get(ident.name);
    if (type) {
      return freshType(type, nonGenericVars);
    } else {
      throw new Error(`Unbound symbol ${ident.name}`);
    }
  }
}

function freshType(type, list) {
  return new CopyEnv().fresh(type, list);
}

class CopyEnv {
  constructor() {
    this.members = [];
  }

  extend(oldType, newType) {
    this.members.push({ oldType: oldType, newType: newType });
  }

  //
  // PROCEDURE Fresh(typeExp: TypeExp; list: NonGenericVars; VAR env: CopyEnv): TypeExp;
  //   BEGIN
  //     typeExp := TypeMod.Prune(typeExp);
  //     CASE typeExp^.class OF
  //     | VarType:
  //         IF IsGeneric(typeExp, list) THEN
  //           RETURN FreshVar(typeExp, env, (*VAR*) env)
  //         ELSE
  //           RETURN typeExp
  //         END;
  //     | OperType:
  //         RETURN
  //           TypeMod.NewTypeOper(typeExp^.ide,
  //             FreshList(typeExp^.args, list, (*VAR*) env));
  fresh(type, list) {
    if (type.class !== 'Type') throw new Error(type.class);
    let env = this;
    type = type.prune();
    return type.visit({
      varType() {
        if (type.isGeneric(list)) {
          return env.freshVar(type);
        } else {
          return type;
        }
      },
      operType() {
        return Type.Oper(type.ide, env.freshList(type.args, list));
      }
    })
  }
  // PROCEDURE FreshVar(typeVar: TypeExp; scan: CopyEnv; VAR env: CopyEnv): TypeExp;
  //   VAR newTypeVar: TypeExp;
  //   BEGIN
  //     IF scan = NIL THEN
  //       newTypeVar := TypeMod.NewTypeVar();
  //       env := ExtendCopyEnv(typeVar, newTypeVar, env);
  //       RETURN newTypeVar;
  //     ELSIF TypeMod.SameType(typeVar, scan^.old) THEN
  //       RETURN scan^.new
  //     ELSE
  //       RETURN FreshVar(typeVar, scan^.tail, (*VAR*) env);
  //     END;
  //   END FreshVar;
  freshVar(type) {
    for (var member in this.members) {
      if (type.SameType(member.old)) {
        return member.new;
      }
    }
    newType = Type.Var();
    this.extend(type, newType);
    return newType;
  }

  // PROCEDURE FreshList(args: TypeList; list: NonGenericVars; VAR env: CopyEnv): TypeList;
  //   BEGIN
  //     IF args = TypeMod.Empty THEN RETURN TypeMod.Empty END;
  //     RETURN
  //       TypeMod.Extend(Fresh(args^.head, list, (*VAR*) env),
  //         FreshList(args^.tail, list, (*VAR*) env));
  //   END FreshList;
  freshList(args, list) {
    return args.map( (arg) => this.fresh(arg, list) );
  }
}
