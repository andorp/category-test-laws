#!/bin/sh

cabal configure --enable-tests --enable-library-coverage
cabal build
cabal test
