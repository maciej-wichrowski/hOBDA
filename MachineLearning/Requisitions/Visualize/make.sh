#!/bin/sh

if [ ! -d dist ]
then
    cabal configure
    echo "------------------"
fi

cabal build

ln -sf dist/build/VisualizeRequisitions/VisualizeRequisitions Visualize