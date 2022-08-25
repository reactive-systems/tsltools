# `tslmt2tsl` Limitations
There are many limitations in synthesizing TSL-MT with `tslmt2tsl`.

The limitations can be categorized in three different types:

## Limitations of the tool
* The temporal atom collection outlined in section 4.1 of the paper is substituted by an approximation.
* The refinement loop given in section 4.4 is not fully implemented.

## Limitations of the dependencies
* As noted in section 5.1, currently (in 2022) state-of-the-art SyGuS solvers such as [CVC5 cannot synthesize recursive functions](https://github.com/cvc5/cvc5/issues/6182).
Therefore, Syntax-Guided Synthesis of a recursive function is replaced with an approximation.

## Limitations of the algorithm

There are many shortcomings (read: open research problems) of the tool.

### Constrained Grammar
Section 4.5 describes some limitations of the grammar, e.g. no support for nested conditionals.

### Simultaneous updates
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

### Discrepancy between pure functions and mutations over time
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