# TSL Tools

A tool set and library for developing and processing [Temporal Stream Logic
(TSL)](https://www.react.uni-saarland.de/publications/FKPS19a.html)
specifications and Control Flow Models (CFMs) that result from TSL
synthesis. 

1. [Tool Overview](#tool-overview)
    * [tslcheck](#tslcheck)
    * [tslsym](#tslsym)
    * [tslsize](#tslsize)
    * [tslresolve](#tslresolve)
    * [tslsplit](#tslsplit)
    * [tsl2tlsf](#tsl2tlsf)
    * [tsl2toml](#tsl2toml)
    * [cfmcheck](#cfmcheck)
    * [cfmsym](#cfmsym)
    * [cfminfo](#cfminfo)
    * [cfm2code](#cfm2code)
    * [tslplay](#tslplay)
    * [tslcoregen](#tslcoregen)
    * [tslminrealizable](#tslminrealizable)

2. [Installation](#installation)
3. [Research and Documentation](#research-and-documentation)
4. [Contributing]()

# Tool Overview

<**TODO** Check if the interfacing is still valid>

## tslcheck

Checks for a set of TSL specification files, whether they are valid or not.

`Usage: tslcheck <files>`

## tslsym

The tool prints the symbol table that is derived from a TSL
specification. The table identifies all inputs, outputs, function, and
predicate symbols, as well as their derived type signatures. Therefore,
the tool provides a first overview of specified modules. It is,
however, also useful to identify typos in the literals used, since they
are automatically introduced by their usage and, thus, do not lead to
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

## tslresolve

<**TODO** Describe>


## tslsplit

<**TODO** Describe>


## tsl2tlsf

The tool under-approximates a TSL specification by a weaker LTL
specification that is given in the [TLSF](https://arxiv.org/abs/1604.02284)
format. The resulting TLSF specification is printed to `STDOUT`.

`Usage: tsl2tlsf <file>`


## tsl2toml 

<**TODO** Describe>


## cfmcheck

Checks for a set of CFM files, whether they are valid or not.

`Usage: cfmcheck <files>`


## cfmsym

Prints the symbol table of a CFM, similar to `tslsym` printing the
symbol table for a TSL specification.

`Usage: cfmsym <file>`


## cfminfo

Prints the number of inputs, outputs, predicates, functions, cells,
and vertices of the generated CFM.

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


## tslplay

<**TODO** Describe>


## tslcoregen

<**TODO** Describe>


## tslminrealizable

<**TODO** Describe>


# Core Generation

<**TODO** This has to go in some kind of documentation>

This library and respective tools generate different kinds cores (minimal 
specification witnesses) for TSL. The following cores exist

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

# Installation

We recommend using the [Haskell Tool Stack](http://haskellstack.org/)
for building. The tool automatically pulls the required version of the 
Glasgow Haskell Compiler (GHC) and all required dependencies. Note that by 
using `stack`, the installation does not interfere with any system 
installation. After `stack` is installed just type

`make`

in the main directory to build TSL tools.

# Research and Documentation

<**TODO:** Documentation part that has to be decided> 

Research:
* [The original paper](https://www.react.uni-saarland.de/publications/FKPS19a.html)
  and its 
  [extended version](https://arxiv.org/abs/1712.00246)
* A FPGA arcade game specified using TSL, 
  [syntroids](https://www.react.uni-saarland.de/casestudies/syntroids/)
* <**TODO**: Gideon's stuff as soon it has been published>
* <**TODO** Is their more stuff that is already public?>

# Contributing

If you want to contribute please refer to [CONTRIBUTING](./CONTRIBUTING.md).
