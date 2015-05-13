Racketeer
=========

A continuous testing plugin for DrRacket.

![Racketeer](./racketeer.gif)

Installation
--------------

#### From DrRacket
1. In DrRacket: File -> Package Manager ->  Do What I Mean
2. Paste this URL into the Package Source textbox: `git://github.com/miraleung/racketeer`
3. Click install

If you don't see Package Manager in the menu, your version of Racket may need to be updated.

#### From [raco](http://docs.racket-lang.org/raco/index.html)
```
raco pkg install racketeer
```

#### From source with raco

This will allow local changes to the tool's source files to be reflected in DrRacket.
```
git clone https://github.com/miraleung/racketeer.git
cd racketeer
raco link racketeer
raco setup racketeer
```

Racketeer Package Updates
--------------------------
#### From DrRacket
1. File -> Package Manager -> Available From Catalog
2. Click "Update Package List"

#### From raco
```
raco pkg update racketeer
```

Usage
-------
1. Code
2. Look at tests
3. Repeat

#### Notes
- Highlighting can be toggled on or off at File -> Racketeer Test Highlighting
- Test highlighting is cleared while code is being edited.
- Test variants neither defined nor included in the currently-used language and libraries are marked as errors.

#### Currently supported languages and test variants
- [HtDP](http://docs.racket-lang.org/htdp-langs/index.html) and [Racket](http://racket-lang.org/): `check-expect, check-error, check-satisfied, check-range, check-member-of`

  - The Racket language (`#lang racket`) does not include these test variants by default. If you are using this language, please add `(require test-engine/racket-tests)` after the language declaration.

- [PLAI](http://docs.racket-lang.org/plai/plai-scheme.html): `test, test/pred, test/exn`
- [RackUnit](http://docs.racket-lang.org/rackunit/index.html): All `check` variants


Uninstalling
-------------

#### From DrRacket
1. In DrRacket: File -> Package Manager -> Currently Installed
2. Filter for "racketeer"
3. Remove

#### From raco
```
raco pkg remove racketeer
```
