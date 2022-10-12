# Temporal Stream Logic Modulo Theories (TSL-MT)

Synthesis support for [Temporal Stream Logic Modulo Theories (TSL-MT)](https://link.springer.com/chapter/10.1007/978-3-030-99253-8_17) is given by the tool `tslmt2tsl`, according to the paper "[Can Reactive Synthesis and Syntax-Guided Synthesis Be Friends?](https://www.marksantolucito.com/papers/pldi2022.pdf)". `tslmt2tsl` transforms a TSL-MT specification into a TSL specification with assumptions.

For instance, you can specify a TSL-MT specification as follows:
```tslmt
#EUF
always guarantee {
    eq x a <-> [x <- y];
    eq a x <-> [y <- x];
    [x <- y] <-> [y <- x];
}
```
Trying to synthesize this as a raw TSL formula will fail, since `eq` is uninterpreted and a reduction to vanilla TSL will lose the commutativity of `eq`.

Running `tslmt2tsl` on the tool will result in the following TSL specification:
```tsl
assume {
	G !((eq x a) && ((eq a x) && (!(eq x a) && !(eq a x))));
	G !((eq x a) && ((eq a x) && !(eq x a)));
	G !((eq x a) && ((eq a x) && !(eq a x)));
	G !((eq x a) && (!(eq x a) && !(eq a x)));
	G !((eq x a) && !(eq x a));
	G !((eq x a) && !(eq a x));
	G !((eq a x) && (!(eq x a) && !(eq a x)));
	G !((eq a x) && !(eq x a));
	G !((eq a x) && !(eq a x));
}
always guarantee {
    eq x a <-> [x <- y];
    eq a x <-> [y <- x];
    [x <- y] <-> [y <- x];
}
```
Sending this to a reactive synthesis engine will now result in a synthesized model.

**WARNING**: This tool is currently a work in progress; only consistency checking assumptions are generated from the tool.
Implementation of other logics such as Linear Integer Arithmetic (LIA) will come after additional parsing support.

## Getting Started

### Installation
In order to run `tslmt2tsl`, you will need a Satisfiability Modulo Theories (SMT) and Syntax-Guided Synthesis Solver (SyGuS) solver.
The recommend solver is [CVC5](https://cvc5.github.io/).

### Running the tool
The tool takes several arguments (flags are addressed in the next section):
```
Usage: tslmt2tsl [INFILE] [-o|--output OUTFILE] SolverPath ([--predicates] |
                 [--cfg] | [--consistency] | [--sygus] | [--assumptions])
```
For instance, to run `tslmt2tsl`
1. On the TSL-MT specification `~/kitchen-timer.tslmt`
2. Output to `stdout`
3. Using `cvc5` in `/usr/bin`
You run the tool by:
```
./tslmt2tsl ~/kitchen-timer.tslmt /usr/bin/cvc5
```

### Flags
Once you annotate the file with the first-order theory, `tslmt2tsl` will attempt to transform the TSL-MT specification to TSL.
However, if you want more diagnostic information, you can use the following flags:
* `--predicates`: Prints all the predicate literals and their dependent cells and output terminals.
* `--cfg`: Prints the possible Context-Free Grammar (CFG) for all cells and output terminals in the specification.
* `--consistency`: Prints all the consistency checking problems, and if solved, prints the corresponding TSL assumption.

## Supported first-order theories
`tslmt2tsl` can support all first-order theories that a SyGuS solver can solve.
However, we currently only have support for the following using [CVC5](https://cvc5.github.io/), using the naming scheme given by the [SMT2 Language Standard](https://smtlib.cs.uiowa.edu/logics.shtml):

* Uninterpreted Functions (UF)
* Equality and Uninterpreted Functions (EUF)

You can define the logic of by writing `#[LOGIC]` at the top of the TSL-MT specification, i.e. `#EUF`.

Combination of theories such as UFLIA are not straightforward to implement as it requires an automated method for the `tslmt2tsl` to know which signals are uninterpreted and which are integers.
This may require explicit type annotations, and modifying TSL with types is an open problem.