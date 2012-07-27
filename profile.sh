ghc Launcher.hs -rtsopts=all -prof  -caf-all -auto-all  -O2  -o prob && time ./prob +RTS -p -K50M
