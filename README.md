EdiT2-Hakell
==============

Implementations of learning patterns with Haskell. Generates a usable tree with ediT2.
Also checks constraints on a tree.
The haddock's documentation is in dist/doc

Patterns
--------

  - [x] Jigsaw
  - [x] Reciprocal teaching
  - [x] Simulation
  - [x] Editors
  - [ ] Pyramid

Install
-------

###Via cabal
  In the <i>ediT2-haskell</i> directory :
	`cabal install`

###Manually
  In the <i>ediT2-haskell</i> directory :
	`ghc --make ediT2-haskell.hs -XDoAndIfThenElse`



How To Use
----------

`ediT2-haskell <command> [options]`
  
`command` = Name of the pattern (<i>Jigsaw, RT, Simulation, Editor</i>), or <i>Constraint</i> to check constraints on a tree.
  
* `ediT2-haskell [Jigsaw | RT | Simulation | Editor] <information file>`
* `ediT2-haskell Checker <output file> <.t2 file> <constraints file>`

Information file
----------------

Examples of information file can be found in `ediT2-haskell/examples`.

Information files gives the information needed by a pattern to generate a usable tree with ediT2.
The first line must contains the filepath betwenn quotes pointing to the output file `.t2`.
The second line contains information for the pattern in haskell's format.

###Jigsaw

Info { objects = (Activity objects, _, Participant objects, Resource objects,_), themes = [Theme], nbPPG = X, above = Y, below = Z}
where
Theme must be of the form : Theme{name = <"NAME">, resources = [<"RESOURCE NAME">], nbExpert = <NUMBER OF EXPERTS>, lowerMargin = <LOWER MARGIN>, upperMargin = <UPPER MARGIN>, nbResources = <NUMBER OF RESOURCES>}
X = the number of participants per initial group.
Y = above margin tolerated for initial groups.
Z = below margin tolerated for initial groups.

###RT

Info { objects = (_, _, Participant objects, Resource objects, _), nbPPG = X, above = Y, below = Z, uniform = [True | False]}
X = the number of participants per initial group.
Y = above margin tolerated for initial groups.
Z = below margin tolerated for initial groups.

###Simulation

###Editor