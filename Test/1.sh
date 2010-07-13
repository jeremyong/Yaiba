cd ..
cabal install --reinstall --ghc-options=-fforce-recomp
cd -
ghc -rtsopts -threaded -O2 -eventlog -fforce-recomp --make Test
./Test +RTS -N -ls -s