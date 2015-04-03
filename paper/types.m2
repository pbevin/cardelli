(***************************************************************************)
(**************************** DEFINITION MODULES ***************************)
(***************************************************************************)
(***************************************************************************)
DEFINITION MODULE ErrorMod;
PROCEDURE Msg(msg: ARRAY OF CHAR);
(* Print an error message *)
END ErrorMod.
(***************************************************************************)
DEFINITION MODULE SymbolMod;
TYPE
  Ide;
PROCEDURE New(string: ARRAY OF CHAR): Ide;
(* Create a new identifier from a string *)
PROCEDURE Equal(ide1, ide2: Ide): BOOLEAN;
(* Compare two identifiers *)
END SymbolMod.
(***************************************************************************)
DEFINITION MODULE ParseTreeMod;
IMPORT SymbolMod;
FROM SymbolMod IMPORT Ide;
TYPE
  Exp = POINTER TO ExpBase;
  (* Parse tree for expressions *)
  Decl = POINTER TO DeclBase;
  (* Parse tree for declarations *)
  ExpClass = (IdeClass, CondClass, LambClass, ApplClass, BlockClass);
  ExpBase = RECORD
    CASE class: ExpClass OF
    | IdeClass: ide: Ide;
    | CondClass: test, ifTrue, ifFalse: Exp;
    | LambClass: binder: Ide; body: Exp;
    | ApplClass: fun, arg: Exp;
    | BlockClass: decl: Decl; scope: Exp;
    END;
END;
  DeclClass = (DefClass, SeqClass, RecClass);
  DeclBase = RECORD
    CASE class: DeclClass OF
    | DefClass: binder: Ide; def: Exp;
    | SeqClass: first, second: Decl;
    | RecClass: rec: Decl;
    END;
END;

(* Allocation routines for Exp and Decl *)
PROCEDURE NewIdeExp(ide: Ide): Exp;
PROCEDURE NewCondExp(test, ifTrue, ifFalse: Exp): Exp;
PROCEDURE NewLambExp(binder: Ide; body: Exp): Exp;
PROCEDURE NewApplExp(fun, arg: Exp): Exp;
PROCEDURE NewBlockExp(decl: Decl; scope: Exp): Exp;
PROCEDURE NewDefDecl(binder: Ide; def: Exp): Decl;
PROCEDURE NewSeqDecl(first, second: Decl): Decl;
PROCEDURE NewRecDecl(rec: Decl): Decl;
END ParseTreeMod.
(***************************************************************************)
DEFINITION MODULE TypeMod;
IMPORT SymbolMod;
FROM SymbolMod IMPORT Ide;
TYPE
  TypeExp = POINTER TO TypeExpBase;
  (* The internal representation of type expressions *)
  TypeClass = (VarType, OperType);
  TypeExpBase = RECORD
    CASE class: TypeClass OF
    | VarType: instance: TypeExp;
    | OperType: ide: Ide; args: TypeList;
    END;
  END;
  TypeList = POINTER TO TypeListBase;
  TypeListBase = RECORD head: TypeExp; tail: TypeList; END;
PROCEDURE NewTypeVar(): TypeExp;
(* Allocate a new type variable *)
PROCEDURE NewTypeOper(ide: Ide; args: TypeList): TypeExp;
(* Allocate a new type operator *)
VAR
  Empty: TypeList;
  (* The empty type list *)
PROCEDURE Extend(head: TypeExp; tail: TypeList): TypeList;
(* Allocate a new type list *)
PROCEDURE SameType(typeExp1, typeExp2: TypeExp): BOOLEAN;
(* Compare two types for identity (pointer equality) *)
PROCEDURE Prune(typeExp: TypeExp): TypeExp;
(* Eliminate redundant instantiated variables at the top of "typeExp";
   The result of Prune is always a non-instantiated type variable or a
   type operator *)
PROCEDURE OccursInType(typeVar: TypeExp; typeExp: TypeExp): BOOLEAN;
(* Whether an uninstantiated type variable occurs in a type expression *)
PROCEDURE OccursInTypeList(typeVar: TypeExp; list: TypeList): BOOLEAN;
(* Whether an uninstantiated type variable occurs in a list *)
PROCEDURE UnifyType(typeExp1, typeExp2: TypeExp);

(* Unify two type expressions *)
PROCEDURE UnifyArgs(list1, list2: TypeList);
(* Unify two lists of type expressions *)
END TypeMod.
(***************************************************************************)
DEFINITION MODULE GenericVarMod;
IMPORT TypeMod;
FROM TypeMod IMPORT TypeExp;
TYPE
  NonGenericVars;
  (* Lists of non-generic type variables and their instantiations *)
VAR
  Empty: NonGenericVars;
  (* The empty list *)
PROCEDURE Extend(head: TypeExp; tail: NonGenericVars): NonGenericVars;
(* Extend a list *)
PROCEDURE IsGeneric(typeVar: TypeExp; list: NonGenericVars): BOOLEAN;
(* Whether an uninstantiated type variable is generic w.r.t. a list of
   non-generic type variables *)
PROCEDURE FreshType(typeExp: TypeExp; list: NonGenericVars): TypeExp;
(* Make a copy of a type expression; the generic varibles are copied, while
   the non-generic variables are shared *)
END GenericVarMod.
(***************************************************************************)
DEFINITION MODULE EnvMod;
IMPORT SymbolMod, TypeMod, GenericVarMod;
FROM SymbolMod IMPORT Ide;
FROM TypeMod IMPORT TypeExp;
FROM GenericVarMod IMPORT NonGenericVars;
TYPE Env;
  (* Environments associating type expressions to identifiers *)
VAR
  Empty: Env;
  (* The empty environment *)
PROCEDURE Extend(ide: Ide; typeExp: TypeExp; tail: Env): Env;
(* Extend an environment with an identifier-type pair *)
PROCEDURE Retrieve(ide: Ide; env: Env; list: NonGenericVars): TypeExp;
(* Search for an identifier in an environment and return a "fresh" copy of
   the associated type (using GenericVar.FreshType). The identifier must be
   bound in the environment *)
END EnvMod.
(***************************************************************************)
DEFINITION MODULE TypecheckMod;
IMPORT ParseTreeMod, TypeMod, EnvMod, GenericVarMod;
FROM ParseTreeMod IMPORT Exp, Decl;
FROM TypeMod IMPORT TypeExp;
FROM EnvMod IMPORT Env;
FROM GenericVarMod IMPORT NonGenericVars;
PROCEDURE AnalyzeExp(exp: Exp; env: Env; list: NonGenericVars): TypeExp;
(* Typecheck an expression w.r.t. an environment, and return its type *)
PROCEDURE AnalyzeDecl(decl: Decl; env: Env; list: NonGenericVars): Env;
(* Typecheck a declaration w.r.t an environment, and return an extended
   environment containing the types of the identifiers introduced by the
   declaration *)
END TypecheckMod.

(***************************************************************************)
(************************* IMPLEMENTATION MODULES **************************)
(***************************************************************************)
(***************************************************************************)
IMPLEMENTATION MODULE TypeMod;
IMPORT ErrorMod;
PROCEDURE NewTypeVar(): TypeExp;
  VAR r: TypeExp;
  BEGIN
    NEW(r, VarType); r^.class := VarType; r^.instance := NIL; RETURN r;
  END NewTypeVar;
PROCEDURE NewTypeOper(ide: Ide; args: TypeList): TypeExp;
  VAR r: TypeExp;
  BEGIN
    NEW(r, OperType); r^.class := OperType; r^.ide := ide; r^.args := args;  RETURN r;
  END NewTypeOper;
PROCEDURE Extend(head: TypeExp; tail: TypeList): TypeList;
  VAR r: TypeList;
  BEGIN
    NEW(r); r^.head := head; r^.tail := tail; RETURN r;
  END Extend;
PROCEDURE SameType(typeExp1, typeExp2: TypeExp): BOOLEAN;
  BEGIN RETURN typeExp1 = typeExp2; END SameType;
PROCEDURE Prune(typeExp: TypeExp): TypeExp;
  BEGIN
    CASE typeExp^.class OF
    | VarType:
        IF typeExp^.instance = NIL THEN
          RETURN typeExp;
        ELSE
          typeExp^.instance := Prune(typeExp^.instance);
          RETURN typeExp^.instance;
        END;
    | OperType: RETURN typeExp;
    END;
END Prune;
PROCEDURE OccursInType(typeVar: TypeExp; typeExp: TypeExp): BOOLEAN;
  BEGIN
    typeExp := Prune(typeExp);
    CASE typeExp^.class OF
    | VarType: RETURN SameType(typeVar, typeExp);
    | OperType: RETURN OccursInTypeList(typeVar, typeExp^.args);
    END;
  END OccursInType;
PROCEDURE OccursInTypeList(typeVar: TypeExp; list: TypeList): BOOLEAN;
  BEGIN

    IF list = NIL THEN RETURN FALSE END;
    IF OccursInType(typeVar, list^.head) THEN RETURN TRUE END;
    RETURN OccursInTypeList(typeVar, list^.tail);
  END OccursInTypeList;
PROCEDURE UnifyType(typeExp1, typeExp2: TypeExp);
  BEGIN
    typeExp1 := Prune(typeExp1);
    typeExp2 := Prune(typeExp2);
    CASE typeExp1^.class OF
    | VarType:
        IF OccursInType(typeExp1, typeExp2) THEN
          IF NOT SameType(typeExp1, typeExp2) THEN
            ErrorMod.Msg("Type clash");
          END;
        ELSE
          typeExp1^.instance := typeExp2;
        END;
    | OperType:
        CASE typeExp2^.class OF
        | VarType: UnifyType(typeExp2, typeExp1);
        | OperType:
            IF SymbolMod.Equal(typeExp1^.ide, typeExp2^.ide) THEN
              UnifyArgs(typeExp1^.args, typeExp2^.args);
            ELSE
              ErrorMod.Msg("Type clash");
END; END;
    END;
  END UnifyType;
PROCEDURE UnifyArgs(list1, list2: TypeList);
  BEGIN
    IF (list1 = Empty) AND (list2 = Empty) THEN RETURN; END;
    IF (list1 = Empty) OR (list2 = Empty) THEN
      ErrorMod.Msg("Type clash");
    ELSE
      UnifyType(list1^.head, list2^.head);
      UnifyArgs(list1^.tail, list2^.tail);
    END;
  END UnifyArgs;
BEGIN
  Empty := NIL;
END TypeMod.
(***************************************************************************)
IMPLEMENTATION MODULE GenericVarMod;
FROM TypeMod IMPORT TypeClass, TypeList;
TYPE
  NonGenericVars = TypeList;
PROCEDURE Extend(head: TypeExp; tail: NonGenericVars): NonGenericVars;

BEGIN RETURN TypeMod.Extend(head, tail); END Extend;
PROCEDURE IsGeneric(typeVar: TypeExp; list: NonGenericVars): BOOLEAN;
  BEGIN RETURN NOT TypeMod.OccursInTypeList(typeVar, list); END IsGeneric;
TYPE
  CopyEnv = POINTER TO CopyEnvBase;
  CopyEnvBase = RECORD old, new: TypeExp; tail: CopyEnv; END;
PROCEDURE ExtendCopyEnv(old, new: TypeExp; tail: CopyEnv): CopyEnv;
  VAR r: CopyEnv;
  BEGIN
    NEW(r); r^.old := old; r^.new := new; r^.tail := tail; RETURN r;
  END ExtendCopyEnv;
PROCEDURE FreshVar(typeVar: TypeExp; scan: CopyEnv; VAR env: CopyEnv): TypeExp;
  VAR newTypeVar: TypeExp;
  BEGIN
    IF scan = NIL THEN
      newTypeVar := TypeMod.NewTypeVar();
      env := ExtendCopyEnv(typeVar, newTypeVar, env);
      RETURN newTypeVar;
    ELSIF TypeMod.SameType(typeVar, scan^.old) THEN
      RETURN scan^.new
    ELSE
      RETURN FreshVar(typeVar, scan^.tail, (*VAR*) env);
    END;
  END FreshVar;
PROCEDURE Fresh(typeExp: TypeExp; list: NonGenericVars; VAR env: CopyEnv): TypeExp;
  BEGIN
    typeExp := TypeMod.Prune(typeExp);
    CASE typeExp^.class OF
    | VarType:
        IF IsGeneric(typeExp, list) THEN
          RETURN FreshVar(typeExp, env, (*VAR*) env)
        ELSE
          RETURN typeExp
        END;
    | OperType:
        RETURN
          TypeMod.NewTypeOper(typeExp^.ide,
            FreshList(typeExp^.args, list, (*VAR*) env));
  END Fresh;
PROCEDURE FreshList(args: TypeList; list: NonGenericVars; VAR env: CopyEnv): TypeList;
  BEGIN
    IF args = TypeMod.Empty THEN RETURN TypeMod.Empty END;
    RETURN
      TypeMod.Extend(Fresh(args^.head, list, (*VAR*) env),
        FreshList(args^.tail, list, (*VAR*) env));
  END FreshList;

END;
PROCEDURE FreshType(typeExp: TypeExp; list: NonGenericVars): TypeExp;
  VAR env: CopyEnv;
  BEGIN env := NIL; RETURN Fresh(typeExp, list, (*VAR*) env); END FreshType;
BEGIN
  Empty := TypeMod.Empty;
END GenericVarMod.
(***************************************************************************)
IMPLEMENTATION MODULE EnvMod;
IMPORT ErrorMod;
TYPE
  Env = POINTER TO EnvBase;
  EnvBase = RECORD ide: Ide; typeExp: TypeExp; tail: Env; END;
PROCEDURE Extend(ide: Ide; typeExp: TypeExp; tail: Env): Env;
  VAR r: Env;
  BEGIN
    NEW(r); r^.ide := ide; r^.typeExp := typeExp; r^.tail := tail; RETURN r;
  END Extend;
PROCEDURE Retrieve(ide: Ide; env: Env; list: NonGenericVars): TypeExp;
  BEGIN
    IF env = EnvMod.Empty THEN
      ErrorMod.Msg("Unbound ide");
      RETURN NIL;
    ELSIF SymbolMod.Equal(ide, env^.ide) THEN
      RETURN GenericVarMod.FreshType(env^.typeExp, list);
    ELSE
      RETURN Retrieve(ide, env^.tail, list);
    END;
  END Retrieve;
BEGIN
  Empty := NIL;
END EnvMod.
(***************************************************************************)
IMPLEMENTATION MODULE TypecheckMod;
IMPORT SymbolMod;
FROM ParseTreeMod IMPORT ExpClass, DeclClass;
FROM TypeMod IMPORT NewTypeVar, NewTypeOper, UnifyType, UnifyArgs;
VAR
  BoolType: TypeExp;
PROCEDURE FunType(dom, cod: TypeExp): TypeExp;
  BEGIN
    RETURN
      NewTypeOper(SymbolMod.New("->"),
        TypeMod.Extend(dom, TypeMod.Extend(cod, TypeMod.Empty)))
  END FunType;

PROCEDURE AnalyzeExp(exp: Exp; env: Env; list: NonGenericVars): TypeExp;
  VAR
    typeOfThen, typeOfElse, typeOfBinder, typeOfBody, typeOfFun, typeOfArg,
    typeOfRes: TypeExp;
    bodyEnv, declEnv: Env;
    bodyList: NonGenericVars;
  BEGIN
    CASE exp^.class OF
    | IdeClass: RETURN EnvMod.Retrieve(exp^.ide, env, list);
    | CondClass:
        UnifyType(AnalyzeExp(exp^.test, env, list), BoolType);
        typeOfThen := AnalyzeExp(exp^.ifTrue, env, list);
        typeOfElse := AnalyzeExp(exp^.ifFalse, env, list);
        UnifyType(typeOfThen, typeOfElse);
        RETURN typeOfThen;
    | LambClass:
        typeOfBinder := NewTypeVar();
        bodyEnv := EnvMod.Extend(exp^.binder, typeOfBinder, env);
        bodyList := GenericVarMod.Extend(typeOfBinder, list);
        typeOfBody := AnalyzeExp(exp^.body, bodyEnv, bodyList);
        RETURN FunType(typeOfBinder, typeOfBody);
    | ApplClass:
        typeOfFun := AnalyzeExp(exp^.fun, env, list);
        typeOfArg := AnalyzeExp(exp^.arg, env, list);
        typeOfRes := NewTypeVar();
        UnifyType(typeOfFun, FunType(typeOfArg, typeOfRes));
        RETURN typeOfRes;
    | BlockClass:
        declEnv := AnalyzeDecl(exp^.decl, env, list);
        RETURN AnalyzeExp(exp^.scope, declEnv, list);
    END;
  END AnalyzeExp;
PROCEDURE AnalyzeDecl(decl: Decl; env: Env; list: NonGenericVars): Env;
  BEGIN
    CASE decl^.class OF
    | DefClass:
        RETURN
          EnvMod.Extend(decl^.binder, AnalyzeExp(decl^.def, env, list), env);
    | SeqClass:
        RETURN
          AnalyzeDecl(decl^.second, AnalyzeDecl(decl^.first, env, list), list);
    | RecClass:
        AnalyzeRecDeclBind(decl^.rec, (*VAR*) env, (*VAR*) list);
        AnalyzeRecDecl(decl^.rec, env, list);
        RETURN env;
    END;
  END AnalyzeDecl;
PROCEDURE AnalyzeRecDeclBind(decl: Decl; VAR env: Env; VAR list: NonGenericVars);
  VAR newTypeVar: TypeExp;
  BEGIN
    CASE decl^.class OF
    | DefClass:
        newTypeVar := NewTypeVar();
        env := EnvMod.Extend(decl^.binder, newTypeVar, env);
        list := GenericVarMod.Extend(newTypeVar, list);
    | SeqClass:
        AnalyzeRecDeclBind(decl^.first, (*VAR*) env, (*VAR*) list);
        AnalyzeRecDeclBind(decl^.second, (*VAR*) env, (*VAR*) list);
    | RecClass: AnalyzeRecDeclBind(decl^.rec, (*VAR*) env, (*VAR*) list);
    END;
  END AnalyzeRecDeclBind;
PROCEDURE AnalyzeRecDecl(decl: Decl; env: Env; list: NonGenericVars);
  BEGIN
    CASE decl^.class OF
    | DefClass:
        UnifyType(EnvMod.Retrieve(decl^.binder, env, list),
          AnalyzeExp(decl^.def, env, list));
    | SeqClass:
        AnalyzeRecDecl(decl^.first, env, list);
        AnalyzeRecDecl(decl^.second, env, list);
    | RecClass: AnalyzeRecDecl(decl^.rec, env, list);
    END;
  END AnalyzeRecDecl;
BEGIN
  BoolType := NewTypeOper(SymbolMod.New("bool"), TypeMod.Empty);
END TypecheckMod.
