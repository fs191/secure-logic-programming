# Transforming Datalog program to a privacy-preserving application based on Sharemind



## Building

### Installing dependencies

#### Debian

    apt install z3

#### Arch Linux

    pacman -Sy z3

### Building with Stack (recommended)

Simply run

    stack install

### Building with Cabal

Building for the first time:

    cabal sandbox init
    cabal update
    cabal install --only-dependencies
    cabal configure
    cabal build

Later, only `cabal build` is required to rebuild when files have changed.

If dependencies or project structure has changed then

    cabal install --only-dependencies
    cabal configure
    cabal build

may be necessary.

## Testing

The testing suite relies on SWI-Prolog and Sharemind emulator. To make it easier
to set up the environment, we use Docker. To run the tests, install Docker and run

    docker build --tag lpsec:latest .
    docker run -it lpsec:latest

## Example run

The exact location of the executables `lpsec` and `lpsec-gen-tables` is shown when running `stack install` or `cabal build`, e.g. `dist/build/lpsec`.

The following examples read datalog file `auction.pl` and create SecreC file `auction.sc`
The parameter `n` is the upper bound on the number of iterations.

    ./dist/build/lpsec/lpsec examples/ppdatalog/auction.pl -o SecreC/auction.sc -n 2

To create a script that uploads simulated data into Sharemind, use `lpsec-gen-tables`.
**WARNING: do not use it to upload real private data, as it is done in public!**

    ./dist/build/lpsec-gen-tables/lpsec-gen-tables examples/ppdatalog/auction.pl SecreC/createdb_auction.sc


The obtained `*.sc` programs can be run on [Sharemind](https://sharemind.cyber.ee/sharemind-mpc/). First of all, `*.sc` programs should be compiled to `*.sb` using SecreC compiler (provided as a part of Sharemind platform). During compilation, the file `lp_essentials.sc` found in the `SecreC` subdirectory of this project should be located in the same place where the compiled `*.sc` file does.

The compiled bytecode can now be run with Sharemind Emulator using the auxiliary Python script, found in the `SecreC` subdirectory of this project.

    python run_sharemind.py --sb=createdb_auction.sb
    python run_sharemind.py --sb=auction.sb

The same code can as well be run on real Sharemind MPC by adding flag --real. For this, a set of computing servers (miners) should be set up first. The instructions for this are provided as a part of Sharemind platform.

    python run_sharemind.py --real --sb=createdb_auction.sb
    python run_sharemind.py --real --sb=auction.sb

Non-privacy-preserving versions of the programs of `examples/ppdatalog` can be found in `examples/prolog`, and can be executed using `test_script.pl` as follows:

    swipl -s test_script.pl filename_without_extension arg_1 ... arg_n

For example

    swipl -s test_script.pl auction
    swipl -s test_script.pl employee
    swipl -s test_script.pl relatives
    swipl -s test_script.pl fib
    swipl -s test_script.pl market alice garlic

# Documentation

The guide is available [here](https://github.com/fs191/secure-logic-programming/blob/master/docs/guide.md).
