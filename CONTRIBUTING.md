# Contribution Guidelines

1. [Reporting a bug / Requesting a feature](#reproting-a-bug-or-requesting-a-feature)
2. [Getting involved](#getting-involved)    
    * [Before getting started](#before-getting-started)
    * [Project structure](#project-structure)
    * [Code Style and Documentation](#code-style-and-documentation)


## Reporting a bug or Requesting a feature

If you have found a bug, you can report it to use by opening a new issue 
labeled with the 'bug' tag. In your problem description please include the 
following, such that we can reproduce the bug:
* the tool(s) or library parts which you used
* the input you provided to the tool(s) or library function(s)
* the error message / output you got (if present)
* the expected behavior and output


If there is a feature that you would like to feel free to
[get involved](#getting-involved). You can also open an issue tagged with 
'feature request'. Please include the following in your feature request:
* the kind of feature, like is is an API-extension, a library extension, a tool
* why you need this feature and maybe what you intend to to with it
* a precise description of the feature: what do you want to be included, what 
  behavior do you expect

Please keep in mind that we are not full-time maintaining this project and 
probably will not have the time for unreasonable complex extensions.
If you are not sure, whether something should/can be added to `tsltools`, feel
free to contact us or open an issue with the tag `discussion`.


## Getting involved

We are always happy for contributions, be it documentation, code improvements 
and new features. You can do this my submitting a pull request. If you are not
sure, whether something might be useful or is in the scope of this project 
(or should be a separate project instead) feel free to submit an issue labeled
with `discussion` or to contact us directly. If you want to get fully involved
with `tsltools` or add a more complex extension, feel free to contact us 
directly, per e-mail. You can also contact us if you have any further question
concerning this project and how to contribute.

Please note that all you contributions will be licensed together with this 
project. The licence can be found in the LICENCE file.


### Before getting started

`tsltools` is developed in Haskell. As building framework we use 
[`stack`](https://docs.haskellstack.org/en/stable/README/). In addition, for
code formatting we use 
[`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell)
(version 0.12.0.0 or higher, you may have to build it from source). 
Furthermore, we recommend to use linting tools such as 
[`hlint`](https://hackage.haskell.org/package/hlint)
as the usually help to write better code.


### Project Structure

The project configuration can be found inside the `tsl.cabal` file (and the
`stack.yaml` file). If you want to add something to the project you have to add
them there, therefore you should probably familiarize yourself with these.
For building we provide a makefile, that includes the following features, 
among others:
* `make` builds the project. 
* `make install` builds the project and install the tools into your local 
  install path (usually `.local/bin/`). This operation does not require 
  root privileges. Note that you might have to include your local install path
  into PATH.
* `make format` reformats the source code.
* `make doc` builds the haddock documentation.

`tsltools` is mainly split in two parts the *library* and the *tools*. The 
*library* contains all core functionalities of `tsltools`, e.g. TSL related
algorithms and data structures, file parsers and printers .... The *tools* on
the other hand a small wrappers around these functionalities that do basic IO 
operations like reading files and parsing command line arguments which are 
normally IO operation that are not inherently connected to the goal of this 
project. They allow a human to use the top-level functioanlities of `tsltools` 
directly. This kind of program organisation allows to include the `tsltools` 
functionalities directly in other projects as most parts of the project are 
already designed as library.

Folder structure:
* `src/` contains the source code of the project.
* `src/lib/` contains the source code of the library part of the project, i.e. 
  the main program logic of `tsltools`. On the top-level of `src/lib` the 
  export modules for the different sub-libraries can be found. Each sub-library
  then has its own sub-folder.
* `src/tools/` contains the code of the individual tools. Each tools has a
  separate folder which usually contains one or two source files. 
* `src/tool-utilities/` contains common routines used by the individual tools. 
  Note that these allow that the different tools are accessed in a similar 
  manner. Hence, when writing a new tool you should take these into 
  consideration.
* `src/tests/`contains unit tests.


### Code Style and Documentation

Good documentation and a consistent code style are important for collaborative
programming projects. Hence, our projects follows the following guidelines:

* For code formatting we use `stylish-haskell` with a custom configuration. To
  reformat the code, you can run `make format` in the top-level folder.
* The usage of further linting tools is encouraged.
* For code documentation we use the 
  [`haddock`](https://haskell-haddock.readthedocs.io/en/latest/index.html)
  documentation format. 
* The documentation should enable people who did not write the respective 
  parts to understand and maintain these parts (this might of course also 
  include some external publications, if necessary).
* Each module shall at least be documented with the module name, the authors 
  name and a short description. Furthermore, all functions and data structures 
  that are accessible form outside of the module have to be documented too.
  It is highly encouraged that other functions and data structures are also
  documented.
* Please refrain form putting your e-mail inside the module. Put it –together
  with the name you used inside your modules– inside the `tsl.cabal` file 
  under the *author* field.
* When adding new modules they should be documented in the 
  [overview documentation](./DOCUMENTATION.md).
* The core functionalities (i.e. usually every part of the program logic that 
  is not user  interaction or IO) of a feature shall be part of the `tsltools` 
  library. Functionalities that are intended to be used by a human should be 
  wrapped inside a `tsltools` tool. For more details refer to the
  [project structure](#project-structure).

