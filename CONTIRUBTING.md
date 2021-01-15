#Contribution Guidelines

1. [Reporting a bug / Requesting a feature](#reproting-a-bug-requesting-a-feature)
2. [Getting involved](#getting-involved)    
    * [Before getting started](#before-getting-started)
    * [Project structure](#project-structure)
    * [Code Style and Documentation](#code-style-and-documentation)
3. []

##Reporting a bug / Requesting a feature

''TODO'': Write

* Bug: Open an issue, describe problem: 
* (which tools) or library did you used 
* the input you provided
* the error message and output you got (if present) 
* the expected behavior (or expected output) 

* Feature: Feel free to get involved (next section); open an issue
* What is the feature: API-extension, library extension, tool
* Why do you need this feature
* Precise description, what exactly 
* (do not expect magic?)

##Getting involved

''TODO'': Write

* Always happy for contribution (documentation, improvements and new features)
* Feel free to submit a pull request
* If not sure simply open an issues or contact us directly (LINK to emails, 
  maybe reactive systems website)
* Ask for assistance
* Latter might be good for bigger projects
* Note: everything under MIT

###Before getting started

''TODO'': Write

* Development is done in Haskell
* Get stack (LINK)
* Get stylish-haskell (VERSION), build from source
* Recommended: hlint

###Project Structure

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


###Code Style and Documentation

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

