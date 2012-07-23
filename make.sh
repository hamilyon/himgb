#!/bin/sh
#make.sh

#cabal install  -p --reinstall <package-name>
#ghc -rtsopts=all -fforce-recomp Measure.hs
ghc Launcher.hs -rtsopts=all -prof  -caf-all -o prob

#ghc Launcher.hs -rtsopts=all -fforce-recomp -prof  -caf-all -o prob

