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


