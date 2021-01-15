# Contribution Guidelines

1. [Reporting a bug / Requesting a feature](#reproting-a-bug-requesting-a-feature)
2. [Getting involved](#getting-involved)    
    * [Before getting started](#before-getting-started)
    * [Project structure](#project-structure)
    * [Code Style and Documentation](#code-style-and-documentation)

## Reporting a bug / Requesting a feature

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
* a precise description of the feature (what do you want to be included, what 
  behavior do you expect)

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
directly, per e-mail. 

''TODO'': Write

* Ask for assistance
* Note: everything under MIT

### Before getting started

''TODO'': Write

* Development is done in Haskell
* Get stack (LINK)
* Get stylish-haskell (VERSION), build from source
* Recommended: hlint

### Project Structure

''TODO'': Write

* Explain how to build project/documentation and tests
* tsl.cabal and stack.yaml 
* src/: Source-code
* General seperated in libarary and tools; libaryry provide functionalities, 
  tools are just accessible wrappers
* tools/: Sourcecode of tools; one folder per tools; mostly just a short 
  main-module
* tool-utilities/: Different utilities for common code in tools, like the CLI, 
  and priting of common stuff
* test/: Unit-Test
* lib/: Library; Folder contains export module for differen library parts; 
  sub-folder for each part 
    'TODO' do we really want a full description here? (see below)


### Code Style and Documentation

''TODO'': Write

* Fairly consistent code style and good documentation are important for 
  collaborative work
* Code formatting: sytlish-haskell with custom script, run with CMD
* Linting tools, e.g. hlint are highly encouraged 
* Code documenation: haddock
* At least: Module with module-name, author-name, short and longer description;
  all functions and data structure that are accessible from outside 
* Examples, can be found everywhere (''TODO'' find nice example)
* Document usage in README/CONTRIUBUTING (for high level overview)
* ''TODO'' For more complicated stuff we might want to have a "doc"-folder, 
  maybe even for the not so complicates stuff (a more detailed project 
  overview might be elsewhere or the haddock should be generated differently)

