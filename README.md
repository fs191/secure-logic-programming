# Transforming Datalog program to a privacy-preserving application based on Sharemind

## Stack (recommended)

`stack install`

## Cabal

Building for the first time:

```
cabal sandbox init
cabal update
cabal install --only-dependencies
cabal configure
cabal build
```

Later, only 
`cabal build`
is required to rebuild when files have changed.

If dependencies or project structure has changed then

```
cabal install --only-dependencies
cabal configure
cabal build
```

may be necessary.


## Example run:

The executable is created in the subdirectory dist/build/lpsec, thus to execute it:
    dist/build/lpsec/lpsec

The following examples read datalog file auction.pl and create SecreC file auction.sc
The parameter n is the upper bound on the number of iterations

    ./dist/build/lpsec/lpsec examples/ppdatalog/auction.pl SecreC/auction.sc -n 2

To create a script that uploads simulated data into Sharemind, use --db-create-tables
WARNING: do not use it to upload real private data, as it is done in public!

    ./dist/build/lpsec/lpsec examples/ppdatalog/auction.pl SecreC/auction.sc --db-create-tables -n 2

If only 'yes/no' output is wanted, use --yes-no-only

    ./dist/build/lpsec/lpsec examples/ppdatalog/auction.pl SecreC/auction.sc --yes-no-only -n 2

The obtained *.sc program can be run on Sharemind. No instructions are available for this yet.


Non-privacy-preserving versions of the same programs can be found in examples/prolog, and can be executed using test_script.pl as follows.

  swipl -s test_script.pl filename_without_extension arg_1 ... arg_n

For example:

  swipl -s test_script.pl auction
  swipl -s test_script.pl employee
  swipl -s test_script.pl relatives
  swipl -s test_script.pl fib
  swipl -s test_script.pl market alice garlic

## License

The MIT License

Copyright 2020-2021 Cybernetica AS

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
