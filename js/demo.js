import parse from './src/parse';
import Env from './src/env';
import Type from './src/type';
import typeCheck from './src/typecheck';


let idFunc = parse("fun(x) x");
let idType = typeCheck(idFunc);
console.log("Type of ID function:", idType.toString());

let f = parse("fun (x) let f = (fun(x) x) in f(x)")
console.log("f has type:", typeCheck(f).toString());


let int = Type.create("int");
let bool = Type.create("bool");
let intToBool = Type.arrow(int, bool);
let intToInt = Type.arrow(int, int);
let intToIntToInt = Type.arrow(int, intToInt);

let env = new Env().preload({
  0: int,
  zero: intToBool,
  succ: intToInt,
  pred: intToInt,
  times: intToIntToInt
});

let fact = parse(
  "let rec factorial = fun(n) \
     if zero(n) \
       then succ(0) \
       else times(n)(factorial(pred(n))) \
   in factorial"
);
let factType = typeCheck(fact, env);
console.log("Type of factorial function:", factType.toString());
