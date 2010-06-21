cd ..
cabal install
cd -
ghc -threaded -O2 -eventlog --make Test
./Test +RTS -N -ls -s