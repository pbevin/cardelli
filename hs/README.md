# Cardelli Type Checker

## Setup

```bash
cabal sandbox init
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
```

## Getting a REPL

```
$ cabal run repl
Preprocessing executable 'repl' for cardelli-0.1.0.0...
Running repl...
Fun Type Inferencer
> fun (x) x+1
int->int
> fun(x) pair(x, x)
a->(a,a)
> fun(x) pair(x, x+1)
int->(int,int)
> let f = fun(x) x in pair(f(3), f(true))
(int,bool)
> let rec factorial = fun(n) if n == 0 then 1 else n * factorial(n-1) in factorial
int->int
```

## Tests

Tests are written using Hspec.

### Single Run

```bash
./run-tests.sh
```

### Automatic Tests

For this you need Ruby and the `guard-shell` gem installed.

```bash
guard
```

Now edit and save a test or a source file and the tests will run automatically.
