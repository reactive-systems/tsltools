# Temporal Stream Logic Modulo Theories (TSL-MT)

Synthesis support for [Temporal Stream Logic Modulo Theories (TSL-MT)](https://link.springer.com/chapter/10.1007/978-3-030-99253-8_17) is given by the tool `tslmt2tsl`, according to the paper "[Can Reactive Synthesis and Syntax-Guided Synthesis Be Friends?](https://www.marksantolucito.com/papers/pldi2022.pdf)". `tslmt2tsl` transforms a TSL-MT specification into a TSL specification with assumptions.

In order to specify a TSL-MT specification, define the appropriate first-order theory at the top of the file with a `#`.
For instance, you can specify a TSL-MT specification as follows:

```
#LIA
always guarantee {
    [x <- add x int1()];
    eq x int0() -> X (eq x int1());
}
```

Trying to synthesize this as a raw TSL formula will fail, since vanilla TSL does not know what `add` means, nor what the numbers `0` or `1` mean.

Running `tslmt2tsl` on the tool will result in the following TSL specification:
```tsl
always assume {
	G !((eq x int0()) && ((eq x int1()) && (!(eq x int0()) && !(eq x int1()))));
	G !((eq x int0()) && ((eq x int1()) && !(eq x int0())));
	G !((eq x int0()) && ((eq x int1()) && !(eq x int1())));
	G !((eq x int0()) && (eq x int1()));
	G !((eq x int0()) && (!(eq x int0()) && !(eq x int1())));
	G !((eq x int0()) && !(eq x int0()));
	G !((eq x int1()) && (!(eq x int0()) && !(eq x int1())));
	G !((eq x int1()) && !(eq x int1()));
	G ( ( (eq x int0()) && ([x <- add x int1()]) ) -> X ((eq x int1())) ) ;

}
always guarantee {
    [x <- add x int1()];
    eq x int0() -> X (eq x int1());
}
```
This specification now adds meanings of Linear Integer Arithmetic to the TSL specification.
Sending this to a reactive synthesis engine will now result in a synthesized model.

## Quick Start

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

## Flags
Once you annotate the file with the first-order theory, `tslmt2tsl` will attempt to transform the TSL-MT specification to TSL.
However, if you want more diagnostic information, you can use the following flags:
* `--predicates`: Prints all the predicate literals and their dependent cells and output terminals.
* `--cfg`: Prints the possible Context-Free Grammar (CFG) for all cells and output terminals in the specification.
* `--consistency`: Prints all the consistency checking problems, and if solved, prints the corresponding TSL assumption.
* `--sygus`: Prints all the Syntax-Guided Synthesis problems, and if solved, prints the corresponding TSL assumption.

## Supported first-order theories
`tsltools` can support all first-order theories that a SyGuS solver can solve.
However, we currently only have support for the following using [CVC5](https://cvc5.github.io/), using the naming scheme given by the [SMT2 Language Standard](https://smtlib.cs.uiowa.edu/logics.shtml):

* Uninterpreted Functions (UF)
* Linear Integer Arithmetic (LIA) : `eq`, `add`, `sub`

You can define the logic of by writing `#[LOGIC]` at the top of the TSL-MT specification, i.e. `#LIA`.

Combination of theories such as UFLIA are not straightforward to implement as it requires an automated method for the `tslmt2tsl` to know which signals are uninterpreted and which are integers.
This is likely an easy problem to solve, but remains an open research problem.

More theories can be added in a straight forward way by adding a theory to the [/src/lib/TSL/ModuloTheories/Theories/](../../src/lib/TSL/ModuloTheories/Theories/) directory and defining it in [/src/lib/TSL/ModuloTheories/Theories.hs](../../src/lib/TSL/ModuloTheories/Theories.hs) file.
If you'd like to let me add another theory, please let the developers know;
in particular, most numeric theories should be easy to add.


## Limitations (Optional Material)
Unfortunately, there are many limitations in synthesizing TSL-MT with `tslmt2tsl`.

The limitations can be categorized in three different types:

### Limitations of the tool
* The temporal atom collection outlined in section 4.1 of the paper is substituted by an approximation.
* The refinement loop given in section 4.4 is not fully implemented.

### Limitations of the dependencies
* As noted in section 5.1, currently (in 2022) state-of-the-art SyGuS solvers such as [CVC5 cannot synthesize recursive functions](https://github.com/cvc5/cvc5/issues/6182).
Therefore, Syntax-Guided Synthesis of a recursive function is replaced with an approximation.

### Limitations of the algorithm

There are many shortcomings (read: open research problems) of the tool.

#### Constrained Grammar
Section 4.5 describes some limitations of the grammar, e.g. no support for nested conditionals.

#### Simultaneous updates
There is no clear way of supporting multiple SyGuS problems in a single query.
Multi-function synthesis is supported by the SyGuS standard as well as by `CVC5`,
but since the results are pure, mapping them over time may change the value of the functions.

For instance, a function `f` that modifies that value of `x` and a function `g` that modifies the value of `y` may both take `x` and `y` as arguments.
While the pure implementations of `f(x)` and `g(y)` may satisfy a post-condition, mapped over time, the function applications may interfere with each other and produce an erroneous result.
This is an open research problem; more generally, a Functional Reactive Program (FRP)-targeted synthesis procedure for TSL may be needed.

This means that current procedure does not support simultaneous updates.
Consider the following TSL-MT specification:
```
always guarantee {
	[var1 <- 4] && [var2 <- 5];
	(eq (add var1 var2) 0 ) -> X [eq (add var1 var2) 9];
}
```
The desired environmental assumption is
```
always assume {
	((eq (add var1 var2) 0) && [var1 <- 4] && [var2 <- 5]) -> X (eq (add var1 var2) 9);
}
```
However, since the algorithm only supports one single pure $\mathcal S$, this assumption cannot be generated.

#### Discrepancy between pure functions and mutations over time
Consider the following TSL-MT specification:
```tslmt
always guarantee {
	eq y 0 -> XXX eq y 5;
	[y <- 1] || [y <- 2] || [y <- add y y];
}
```
Using our procedure, we can obtain a Data Transformation Obligation(DTO):
* Pre-condition: `eq y 0`
* Post-condition: `XXX eq y 5`

Which will then synthesize a function:
* Synthesized Function: `add (add (2 1)) 2`

However, unrolling this to a TSL assumption will produce
```tsl
always assume {
	( eq y 0
	  &&    ([y <- 2]       && [y <- y])
	  &&   X([y <- add y y] && [y <- 2])
	  &&  XX([y <- add y y]            )
	) -> XXX eq y 5;
}
```
Which is obviously problematic: multiple updates to `y` are happening in a single time-step.