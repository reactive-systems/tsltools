# Temporal Stream Logic Modulo Theories

Synthesis support for [Temporal Stream Logic Modulo Theories (TSL-MT)](https://link.springer.com/chapter/10.1007/978-3-030-99253-8_17) is given according to the paper "[Can Reactive Synthesis and Syntax-Guided Synthesis Be Friends?](https://www.marksantolucito.com/papers/pldi2022.pdf)".

In order to specify a TSL-MT specification, define the appropriate first-order theory at the top of the file with a `#`.
For instance, the example in the Introduction of the paper can be specified as follows:

```
#LIA
always guarantee {
	eq x 0 -> F (eq x 2);
	[x <- add x 1] || [x <- sub x 1];
}
```

For backwards compatibility, all TSL specifications without an explicit First-Order Theory will be synthesized without theory support.
However, as classic TSL is just TSL-MT with the Theory of Uninterpreted Functions, a `#UF` annotation is recommended when writing a TSL specification.
This allows TSL specifications (equivalently, TSL Modulo the Theory of Uninterpreted Functions) to _not underapproximate_ to Linear Temporal Logic (LTL) during synthesis as the TSL-MT synthesis procedure will capture the semantics of the update operator.
More explanation is given in Example 4.3 of the [TSL-MT synthesis paper](https://www.marksantolucito.com/papers/pldi2022.pdf).

## Supported first-order theories
`tsltools` can support all first-order theories that a SyGuS solver can solve.
However, we currently only have support for the following using [CVC5](https://cvc5.github.io/):

* Uninterpreted Functions (UF)
* Linear Integer Arithmetic (LIA): `eq`, `add`, `sub`

Other first-order theories can easily be included in the system by simple parsing support.
Combining theories (e.g. with two or more first-order theories) should also be a straightforward extension.

## Flags
Once you annotate the file with the first-order theory, `tsltools` will attempt to synthesize the specification with the relevant First-Order Theory support.
However, if you want more diagnostic information, you can use the `--tslmt` flag and combine it with the following other flags:
* `--tslmt --predicates`: Prints all the predicate literals and their dependent cells and output terminals.
* `--tslmt --cfg`: Prints the possible Context-Free Grammar (CFG) for all cells and output terminals in the specification.
* `--tslmt --consistency`: Prints all the consistency checking problems that can be obtained from the specification.
* `--tslmt --consistency --solved`: Prints all the consistency checking problems, and if solved, prints the corresponding TSL assumption.
* `--tslmt --dto`: Prints all the data transformation obligations that can be obtained from the specification.
* `--tslmt --sygus`: Prints all the Syntax-Guided Synthesis problems that can be obtained from the specification.
* `--tslmt --sygus --solved`: Prints all the Syntax-Guided Synthesis problems, and if solved, prints the corresponding TSL assumption.
* `--tslmt --assumptions`: Prints all the assumptions that are generated from the TSL-MT synthesis procedure.
* `--tslmt --totsl`: Transforms a TSL-MT specification to a classic TSL specification, and prints the result.

## Limitations
There are many limitations in synthesizing TSL-MT.
The limitations can be categorized in three different types:

### Limitations of the tool
* The temporal atom collection outlined in section 4.1 of the paper is substituted by an approximation.
* The refinement loop given in section 4.4 is not fully implemented.
### Limitations of the dependencies
* As noted in section 5.1, current (in 2022) state-of-the-art SyGuS solvers such as [CVC5 cannot synthesize recursive functions](https://github.com/cvc5/cvc5/issues/6182).
Therefore, Syntax-Guided Synthesis of a recursive function is replaced with an approximation.
### Limitations of the algorithm
* Section 4.5 describes some limitations of the grammar, e.g. no support for nested conditionals.
* Similarly, the current procedure does not support simultaneous updates.
Consider the following TSL-MT specification:
```
always guarantee {
	[var1 <- 4] && [var2 <- 5];
	(eq (add var1 var2) 0 ) -> X [eq (add var1 var2) 9];
}
```
The correct environmental assumption learned from SyGuS should be 
```
always assume {
	((eq (add var1 var2) 0) && [var1 <- 4] && [var2 <- 5]) -> X (eq (add var1 var2) 9);
}
```
However, since the algorithm only supports one single pure $\mathcal S$, this assumption cannot be generated.
