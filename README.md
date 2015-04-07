Racketeer
=========

A continuous testing plugin for DrRacket.

![Racketeer](./racketeer.gif)

Installation
--------------

#### From DrRacket
1. Download [this file](https://stash.ugrad.cs.ubc.ca:8443/projects/QWERTY/repos/racketeer/browse/racketeer.plt)
2. Open DrRacket: File -> Install .plt file
3. Select racketeer.plt

#### From source with [raco](http://docs.racket-lang.org/raco/index.html)

```
git clone https://s4b8@stash.ugrad.cs.ubc.ca:8443/scm/qwerty/racketeer.git
cd racketeer
raco link racketeer
raco setup racketeer
```

or

```
git clone https://s4b8@stash.ugrad.cs.ubc.ca:8443/scm/qwerty/racketeer.git
cd racketeer
raco pkg install racketeer
```
#### Recompiling from source
```
git clone https://s4b8@stash.ugrad.cs.ubc.ca:8443/scm/qwerty/racketeer.git
cd racketeer
raco pack --collect racketeer.plt racketeer
```
Install the `.plt` file as in the first section above.


Usage
-------
1. Code
2. Look at tests
3. Repeat

Highlighting can be toggled on or off at File -> Racketeer Test Highlighting

#### Currently supported languages and test variants
- [HtDP](http://docs.racket-lang.org/htdp-langs/index.html) and [Racket](http://racket-lang.org/): `check-error, check-expect, check-member-of, check-range, check-satisfied`

  - The Racket language (`#lang racket`) does not include these test variants by default. If you are using this language, please add `(require test-engine/racket-tests)` after the language declaration.

- [PLAI](http://docs.racket-lang.org/plai/plai-scheme.html): `test`, `test/exn`, `test/pred`


Uninstalling
-------------

#### From DrRacket
In DrRacket, go to Preferences -> Tool and disable the plugin.

\* DrRacket doesn't yet have a good way to completely remove plugins.

#### From source
```
raco pkg remove plugin-name
```
