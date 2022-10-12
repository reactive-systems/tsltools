# `tslpreprocess`

`tslpreprocess` is a tool that transforms a "sugared" TSL language into standard TSL.
The "sugared" TSL language can include, in addition to standard TSL constructs:
* Binary operators: `=`, `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `=>`
* Numeric Constants: Reals (e.g. `3.14`) and Ints (e.g. `47`)

## Examples
### Example 1:
```tsl
always guarantee {
x = y;
}
```
Gets transformed to:
```tsl
always guarantee {
eq x y;
}
```

### Example 2:
```tsl
always guarantee {
[x <- 3.14 + 2.72];
}
```
Gets transformed to:
```tsl
always guarantee {
[x <- add real3.14() real2.72() ];
}
```

### Example 3:
```tsl
always guarantee {
(x + y) = (y + x) ;
}
```
Gets transformed to:
```tsl
always guarantee {
eq (add x y) (add y x) ;
}
```


#### **WARNING**: the parser does not support assumptions or guarantees that span multiple lines.
For instance, a specification
```
F (press Button && 
    status True);
```
should be rewritten as 
```
F (press Button && status True);
```
to be handled by the tool.

#### Future extensions
Instead of having `tslpreprocess` as a separate tool, adding a TSL language extension
```hs
{-# DESUGAR #-}
```
will desugar the specification automatically.