# TSL Tools

A tool set for processing [Temporal Stream Logic
(TSL)](https://www.react.uni-saarland.de/publications/FKPS19a.html)
specifications and Control Flow Models (CFMs) that result from TSL
synthesis. The following tools are provided:

## tslcheck

Checks for a set of TSL specification files, whether they are valid or
not.

`Usage: tslcheck <files>`

## tslsym

The tool prints the symbol table that is derived from a TSL
specification. The table identifies all inputs, outputs, function, and
predicate symbols, as well as their derived type signatures. Therfore,
the tool provides a first overview of specified modules. It is,
however, also useful to identfy typos in the literals used, since they
are automatiallcy introduced by their usage and, thus, do not lead to
an error if spelled incorrectly.

`Usage: tslsym [OPTIONS] <file>`

If no input file is given, then it is read from `STDIN`. Possible
options are:

|Option|Description|
|:-|:-|
| `-f, --full` | prints the full table,including locally bound identifiers |
| `-n, --no-positions` | removes the 'Position'-column from the table such that the resulting </br> table is identical to the one of `cfmsym` |

## tslsize

The tool prints the size of the specification, i.e., the number of AST
nodes of the underlying TSL formula. It can be used for comparing a
set of TSL benchmarks.

`Usage: tslsize <file>`

## tsl2tlsf

The tool under-approximates a TSL specification by a weaker LTL
specification that is given in the [TLSF](https://arxiv.org/abs/1604.02284)
format. The resulting TLSF specifiction is printed to `STDOUT`.

`Usage: tsl2tlsf <file>`

## cfmcheck

Checks for a set of CFM files, whether they are valid or not.

`Usage: cfmcheck <files>`

## cfmsym

Prints the symbol table of a CFM, similar to `tslsym` printing the
symbol table for a TSL specification.

`Usage: cfmsym <file>`

## cfminfo

Prints the number of inputs, outputs, predicates, functions, cells,
and vertices of the genrated CFM.

`Usage: cfminfo <file>`

## cfm2code

Generates executable code from a valid CFM. To this end, a specific
code target must be selected. Supported targets are:

| Target | Description |
|:-|:-|
| `applicative` | generates code for Applicative FRP libraries |
| `monadic` | generates code for Monadic FRP libraries |
| `arrow` | generates code for Arrowized FRP libraries |
| `clash` | generates code for the hardware description language [CλaSH](https://clash-lang.org/) |

`Usage: cfm2code [OPTIONS] <target> <file>`

Possible options are:

|Option|Description|
|:-|:-|
| `-o, --output <file>` | path of the output file (results are printed to `STDOUT` if not used |
| `-m, --module-name <string>` | overwrites the name of the generated module |
| `-f, --function-name <string>` | overwrites the name of the exported function |

# Core Generation

''TODO'': Adapt to style of other descriptions

This library and respective tools generate different kinds cores (minimal 
specification witensses) for TSL. The following cores exist

* Unrealizability cores: A unrealizability cores of a specification is a 
  sub-specification with a minimal amount of guarantees and all assumptions 
  such that this sub-specification is unrealizable. 
  They can be computed by ``tslcoregen``.
* Minimal assumption cores: A minimal assuption core of a specification is the 
  sub-specification with all guarantees with a minimal amount of assumptions 
  such that this sub-specification is realizable.
  Note that this repersent then minimal amount of restriction one has to impose
  on the enivronment such that a desired system can be constructed. 
  The can be computed by ``tslminrealizable`` either in a plain or tree-like
  manner.

A description of the alogrithm can be found in the [projects wiki page](https://wiki.projectjarvis.de/index.php/TSL_Core_Generation).


# Installation

We recommend using the [Haskell Tool Stack](http://haskellstack.org/)
for building. The tool automatically pulls the
required version of the Glasgow Haskell Compiler (GHC) and all
required dependencies. Note that by using `stack`, the installation
does not interfer with any system installation. After `stack` is
installed just type

`make`

in the main directory to build TSL tools.


