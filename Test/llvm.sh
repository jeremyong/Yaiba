cd ..
cabal install -O --ghc-options=-fllvm
cd -
ghc -threaded -O2 -eventlog -rtsopts -fllvm -fforce-recomp --make Test
./Test +RTS -N -ls -s