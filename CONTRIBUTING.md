# Contribution Guidelines

1. [Reporting a bug / Requesting a feature](#reporting-a-bug-or-requesting-a-feature)
2. [Getting involved](#getting-involved)
    * [Before getting started](#before-getting-started)
    * [Project structure](#project-structure)
    * [Code Style and Documentation](#code-style-and-documentation)

## Reporting a bug or Requesting a feature

If you identify a bug, you can report it by opening a new issue labeled with the 'bug' tag. For reproduction, please include the following in your problem description:
* the tool(s) or library parts used
* the inputs provided to the tool(s) or library function(s)
* the produced error message or output (if present)
* the expected behavior and output

If there is a feature you like [getting involved](#getting-involved) with, please open an issue tagged with 'feature request' including the following:
* the kind of feature: is it an API-extension, a library extension, or a tool?
* the intentions behind the feature and the use of it
* a precise description of the feature: which functionality should be included, which behavior would you expect?

Please keep in mind that we are not full-time maintaining this project and probably will not have the time for unreasonable complex extensions. If you are not sure, whether something should or can be added to the toolset, feel free to contact us or open an issue with the tag `discussion`.

## Getting involved

We are always happy for contributions, be it documentation, code improvements or new features. You can help us by submitting a pull request. If you are not sure, whether something is useful or in the scope of this project (or should be a separate project instead) feel free to submit an issue labeled with `discussion`. If you want to get fully involved or want to add a more complex extension, feel free to contact us directly. You can also contact us if you have further questions concerning the project and how to contribute.

Please note that all you contributions will be licensed together with this  project. The license can be found in the LICENSE file.

### Before getting started

`tsltools` is developed in Haskell. The building framework, we use, is [`stack`](https://docs.haskellstack.org/en/stable/README/). In addition, for code formatting, we use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell)(version 0.12.0.0 or higher, you may have to build it from source). Furthermore, we recommend to use linting tools such as [`hlint`](https://hackage.haskell.org/package/hlint) to improve overall code quality.

### Project Structure

The project configuration can be found inside the `tsl.cabal` file (and the `stack.yaml` file). If you want to add something to the project you have to add them there, therefore you should probably familiarize yourself with these files first. For easy building we provide a makefile, that includes, among others, the following features:
* `make` builds the project.
* `make install` builds the project and installs the tools into your local install path (usually `.local/bin/`). This operation does not require root privileges. Note that you might have to include your local install path into `PATH`.
* `make format` reformats the source code.
* `make doc` builds the haddock documentation.

`tsltools` is mainly split in two parts: the *library* and the *tools*. The *library* contains all core functionalities of `tsltools`, e.g., TSL related algorithms and data structures, file parsers and printers ... . The *tools* on the other hand are small wrappers around the functionalities provided by the library in combination with the required IO operations, like reading files or parsing command line arguments, which are normally IO operations not inherently connected to the goal of this project. They allow users to use the top-level functionalities of `tsltools` directly. This kind of program organization allows to include functionalities directly in other projects, as most parts of the project are already designed as library.

Folder structure:
* `src/` contains the source code of the project.
* `src/lib/` contains the source code of the library part of the project, i.e., the main program logic of `tsltools`. The export modules for the different sub-libraries are placed at the top-level of `src/lib`. Each sub-library has its own sub-folder.
* `src/tools/` contains the code of the individual tools. Each tools has a separate folder which usually contains one or two source files.
* `src/tool-utilities/` contains common routines used by the individual tools. Note that these allow that the different tools are accessed in a similar manner. Hence, when writing a new tool you should take these into account.
* `src/tests/` contains some unit tests.

### Code Style and Documentation

Good documentation and a consistent code style are important for collaborative programming projects. Therefore, our projects follows the following guidelines:

* For code formatting we use `stylish-haskell` with a custom configuration. To reformat the code, you can run `make format` in the top-level folder.
* The usage of further linting tools is encouraged.
* For code documentation we use the [`haddock`](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation format.
* The documentation should enable people who did not write the respective parts to understand and maintain these parts (this might of course also include some external publications, if necessary).
* Each module shall at least be documented with the module name, the authors name and a short description. Furthermore, all functions and data structures that are accessible form outside of the module have to be documented as well. It is highly encouraged that other functions and data structures are also documented.
* Please refrain from putting your e-mail inside the module. Put it – together with the name you used inside your modules – inside the `tsl.cabal` file under the *author* field.
* When adding new modules they should be documented in the [overview documentation](./DOCUMENTATION.md).
* The core functionalities (i.e. every part of the program logic that is not a user interaction or IO) of a feature shall be part of the `tsltools` library. Functionalities that are intended to be used by a human should be wrapped inside a `tsltools` tool. For more details refer to the [project structure](#project-structure).
