hOBDA
=====

haskell/heuristics Operational Business Decision Algorithm

_TODO short project description_

This code is the result of a thesis project at Chalmers University of 
Technology in collaboration with IFS World. The thesis itself is available on
request.

Contents
--------
_TODO write some stuff here about what each program does and that._
For more details and code documentation, see ``docs/index.html``.

Dependencies
------------
* Haskell (http://www.haskell.org)
* HsCharts (https://github.com/wicher/HsCharts)
* Gloss >= 1.2 (http://hackage.haskell.org/package/gloss)
* MonadRandom >= 0.1.8 (http://hackage.haskell.org/package/MonadRandom)

Building
--------
1. Navigate to MachineLearning/Requisitions
2. Choose a project to build (Generate, InsertErrors, RData, Visualize) and 
   navigate to that directory.
3. Run ``cabal configure``
4. Run ``cabal build``

Alternatively, when using bash, run ``make.sh`` instead of steps 3 and 4. This 
will run cabal and create a symlink to the executable to make running the 
programs a bit easier.

Running
-------
After building, the executables can be found in ``/dist/build/x/`` where x 
is the name of the project (Generate, InsertErrors, etc).
The executables require various command-line arguments. Running it without any 
arguments will bring up info about the required arguments:

    Usage: Visualize csv-file algorithm(A-C)

    Usage: InsertErrors original_set_csv
                        num_errors

    Usage: Generate num_orders
                    num_suppliers
                    num_features
                    [num_pick_features=1]
                    [feature_min=0] [feature_max=100]