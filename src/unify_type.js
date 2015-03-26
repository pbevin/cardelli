import _ from 'lodash';

// PROCEDURE UnifyType(typeExp1, typeExp2: TypeExp);
//   BEGIN
//     typeExp1 := Prune(typeExp1);
//     typeExp2 := Prune(typeExp2);
//     CASE typeExp1^.class OF
//     | VarType:
//         IF OccursInType(typeExp1, typeExp2) THEN
//           IF NOT SameType(typeExp1, typeExp2) THEN
//             ErrorMod.Msg("Type clash");
//           END;
//         ELSE
//           typeExp1^.instance := typeExp2;
//         END;
//     | OperType:
//         CASE typeExp2^.class OF
//         | VarType: UnifyType(typeExp2, typeExp1);
//         | OperType:
//             IF SymbolMod.Equal(typeExp1^.ide, typeExp2^.ide) THEN
//               UnifyArgs(typeExp1^.args, typeExp2^.args);
//             ELSE
//               ErrorMod.Msg("Type clash");
//   END; END; END;
// END UnifyType;
export default function unifyType(type1, type2) {
  type1 = type1.prune();
  type2 = type2.prune();

  type1.visit({
    varType() {
      if (type1.occursInType(type2)) {
        if (!type1.sameType(type2)) {
          throw new Error("Type clash");
        }
      } else {
        type1.instance = type2;  // XXX mutable type1
      }
    },

    operType() {
      type2.visit({
        varType() { unifyType(type2, type1) },
        operType() {
          if (type1.ide.equals(type2.ide)) {
            unifyArgs(type1.args, type2.args);
          } else {
            throw new Error("Type clash");
          }
        }
      });
    }
  });
}

// PROCEDURE UnifyArgs(list1, list2: TypeList);
//   BEGIN
//     IF (list1 = Empty) AND (list2 = Empty) THEN RETURN; END;
//     IF (list1 = Empty) OR (list2 = Empty) THEN
//       ErrorMod.Msg("Type clash");
//     ELSE
//       UnifyType(list1^.head, list2^.head);
//       UnifyArgs(list1^.tail, list2^.tail);
//     END;
//   END UnifyArgs;
function unifyArgs(args1, args2) {
  if (args1.length != args2.length) {
    throw new Error("Type clash");
  }

  _.zip(args1.types, args2.types).forEach( (types) => { unifyType.apply(null, types); });
}

