import Parsimmon from 'parsimmon';
import AST from './ast';
import Symbol from './symbol';

let { string, regex, optWhitespace, seq, lazy, alt, succeed, fail } = Parsimmon;

function lexeme(p) { return p.skip(optWhitespace); }

let FUN  = lexeme(string("fun"));
let LET  = lexeme(string("let"));
let REC  = lexeme(string("rec"));
let IN   = lexeme(string("in"));
let IF   = lexeme(string("if"));
let THEN = lexeme(string("then"));
let ELSE = lexeme(string("else"));
let LPAR  = lexeme(string("("));
let RPAR  = lexeme(string(")"));
let EQ    = lexeme(string("="));

function mkIdent(id) {
  return AST.Ide(new Symbol(id));
}

let failReserved = alt(FUN, LET, REC, IN, IF, THEN, ELSE).chain(() => fail("reserved word not expected"))

// Manually eliminating left recursion on rule: Expr -> Expr ( Expr )
let expr = lazy("an expression", function() {
  return alt(lambda, cond, letExpr, parenExpr, atom, number).chain(function(lhs) {
    function apply(l, r) { return AST.Apply(l, r) }
    return parenExpr.many().map((r) => r.reduce(apply, lhs));
  })
});
let decl = lazy("a declaration", () => alt(recursion, parenDecl, assign));

let number    = lexeme(regex(/[0-9]+/).map(mkIdent));
let atom      = lexeme(regex(/[a-z_]\w*/i)).map(mkIdent);
let lambda    = seq(FUN, LPAR, atom, RPAR, expr).map((r) => AST.Lambda(r[2], r[4]));
let cond      = seq(IF, expr, THEN, expr, ELSE, expr).map((r) => AST.Cond(r[1], r[3], r[5]));
let letExpr   = seq(LET, decl, IN, expr).map((r) => AST.Block(r[1], r[3]));
let parenExpr = seq(LPAR, expr, RPAR).map((r) => r[1]);

let assign    = seq(atom, EQ, expr).map((r) => AST.Def(r[0].ide, r[2]));
let recursion = seq(REC, decl).map((r) => AST.Rec(r[1]));
let parenDecl = seq(LPAR, decl, RPAR).map((r) => r[1]);

export default function parse(input) {
  let result = optWhitespace.then(expr).parse(input);
  if (result.value) {
    return result.value;
  }

  console.log(Parsimmon.formatError(input, result))
  throw new Error();
}
