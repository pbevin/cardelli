import Env from './env';
import NonGenericVars from './nonGenericVars';
import Type from './type';
import TypeList from './typelist';
import unifyType from './unify_type';
import Symbol from './symbol';

let boolType = Type.Oper(new Symbol("bool"));

export default function typeCheck(ast, env) {
  if (!env) env = new Env();

  return ast.visit(new Analyzer(env, new NonGenericVars()));
}

class AbstractAnalyzer {
  constructor(env, nonGenericVars) {
    this.env = env;
    this.nonGenericVars = nonGenericVars;
  }
}

class Analyzer extends AbstractAnalyzer {
  //     | IdeClass: RETURN EnvMod.Retrieve(exp^.ide, env, list);
  ide(ide) {
    return this.env.retrieve(ide, this.nonGenericVars);
  }

  //     | CondClass:
  //         UnifyType(AnalyzeExp(exp^.test, env, list), BoolType);
  //         typeOfThen := AnalyzeExp(exp^.ifTrue, env, list);
  //         typeOfElse := AnalyzeExp(exp^.ifFalse, env, list);
  //         UnifyType(typeOfThen, typeOfElse);
  //         RETURN typeOfThen;
  cond(test, ifTrue, ifFalse) {
    unifyType(test.visit(this), boolType);
    let typeOfThen = ifTrue.visit(this);
    let typeOfElse = ifFalse.visit(this);
    unifyType(typeOfThen, typeOfElse);

    return typeOfThen;
  }

  //     | LambClass:
  //         typeOfBinder := NewTypeVar();
  //         bodyEnv := EnvMod.Extend(exp^.binder, typeOfBinder, env);
  //         bodyList := GenericVarMod.Extend(typeOfBinder, list);
  //         typeOfBody := AnalyzeExp(exp^.body, bodyEnv, bodyList);
  //         RETURN FunType(typeOfBinder, typeOfBody);
  lambda(binder, body) {
    let typeOfBinder = Type.Var();
    let bodyEnv = this.env.extend(binder.ide, typeOfBinder);
    let bodyList = this.nonGenericVars.extend(typeOfBinder);
    let typeOfBody = body.visit(new Analyzer(bodyEnv, bodyList));
    return funType(typeOfBinder, typeOfBody);
  }


  //     | ApplClass:
  //         typeOfFun := AnalyzeExp(exp^.fun, env, list);
  //         typeOfArg := AnalyzeExp(exp^.arg, env, list);
  //         typeOfRes := NewTypeVar();
  //         UnifyType(typeOfFun, FunType(typeOfArg, typeOfRes));
  //         RETURN typeOfRes;
  apply(fun, arg) {
    let typeOfFun = fun.visit(this);
    let typeOfArg = arg.visit(this);
    let typeOfRes = Type.Var();

    unifyType(typeOfFun, funType(typeOfArg, typeOfRes));
    return typeOfRes;
  }



  //     | BlockClass:
  //         declEnv := AnalyzeDecl(exp^.decl, env, list);
  //         RETURN AnalyzeExp(exp^.scope, declEnv, list);
  block(decl, scope) {
    let declEnv = decl.visit(this);
    return scope.visit(new Analyzer(declEnv, this.nonGenericVars));
  }

  //     | DefClass:
  //         RETURN
  //           EnvMod.Extend(decl^.binder, AnalyzeExp(decl^.def, env, list), env);
  def(binder, def) {
    return this.env.extend(binder, def.visit(this));
  }

  //     | SeqClass:
  //         RETURN
  //           AnalyzeDecl(decl^.second, AnalyzeDecl(decl^.first, env, list), list);
  seq(first, second) {
    let firstEnv = first.visit(this);
    return this.second.analyze(new Analyzer(firstEnv, this.nonGenericVars));
  }

  //     | RecClass:
  //         AnalyzeRecDeclBind(decl^.rec, (*VAR*) env, (*VAR*) list);
  //         AnalyzeRecDecl(decl^.rec, env, list);
  //         RETURN env;
  rec(rec) {
    let { env, nonGenericVars } = rec.visit(new AnalyzeRecDeclBind(this.env, this.nonGenericVars));
    rec.visit(new AnalyzeRecDecl(env, nonGenericVars));
    return env;
  }
}

class AnalyzeRecDeclBind extends AbstractAnalyzer {
  //     | DefClass:
  // newTypeVar := NewTypeVar();
  //         env := EnvMod.Extend(decl^.binder, newTypeVar, env);
  //         list := GenericVarMod.Extend(newTypeVar, list);
  def(binder, def) {
    let type = Type.Var();
    return {
      env: this.env.extend(binder, type),
      nonGenericVars: this.nonGenericVars.extend(type)
    };
  }

  //     | SeqClass:
  //         AnalyzeRecDeclBind(decl^.first, (*VAR*) env, (*VAR*) list);
  //         AnalyzeRecDeclBind(decl^.second, (*VAR*) env, (*VAR*) list);
  seq(first, second) {
    let { env, nonGenericVars } = first.visit(this);
    return second.visit(new AnalyzeRecDeclBind(env, nonGenericVars));
  }

  //     | RecClass: AnalyzeRecDeclBind(decl^.rec, (*VAR*) env, (*VAR*) list);
  rec(rec) {
    return rec.visit(this);
  }
}

class AnalyzeRecDecl extends AbstractAnalyzer {
  //     | DefClass:
  //         UnifyType(EnvMod.Retrieve(decl^.binder, env, list),
  //           AnalyzeExp(decl^.def, env, list));
  def(binder, def) {
    unifyType(
      this.env.retrieve(binder, this.nonGenericVars),
      def.visit(new Analyzer(this.env, this.nonGenericVars))
    );
  }

  //     | SeqClass:
  //         AnalyzeRecDecl(decl^.first, env, list);
  //         AnalyzeRecDecl(decl^.second, env, list);
  seq(first, second) {
    first.visit(this);
    second.visit(this);
  }

  //     | RecClass: AnalyzeRecDecl(decl^.rec, env, list);
  rec(rec) {
    rec.visit(this);
  }
}

// PROCEDURE FunType(dom, cod: TypeExp): TypeExp;
//   BEGIN
//     RETURN
//       NewTypeOper(SymbolMod.New("->"),
//         TypeMod.Extend(dom, TypeMod.Extend(cod, TypeMod.Empty)))
//   END FunType;
function funType(domain, codomain) {
  if (!codomain) throw new Error();
  return Type.Oper(new Symbol("->"), new TypeList(domain, codomain));
  // return Type.Oper(domain, new TypeList(codomain));
}
