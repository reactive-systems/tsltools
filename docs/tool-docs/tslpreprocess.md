# `tslpreprocess`

`tslpreprocess` is a tool that transforms a "sugared" TSL language into standard TSL.
The "sugared" TSL language can include, in addition to standard TSL constructs:
* Binary operators: `=`, `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `=>`
* Numeric Constants: Reals (e.g. `3.14`) and Ints (e.g. `47`)


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
will automatically desugar the specification automatically.