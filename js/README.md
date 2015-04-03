# Cardelli Type Checker

This is a reasonably faithful translation of the Modula-2 code
from [_Basic Polymorphic Typechecking_][1] by Luca Cardelli.

## Installation

```bash
git clone https://github.com/pbevin/cardelli
cd cardelli
npm install
```

## Usage

```bash
npm run test
npm run demo
```

The [demo.js](demo.js) script shows that the type checker can determine
the type of the `id` function (`a -> a`) and the `factorial` function
as defined in the paper.


## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D


[1]: http://lucacardelli.name/Papers/BasicTypechecking.pdf
