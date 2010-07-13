cd ..
cabal install --reinstall
cd -
ghc -threaded -O2 -eventlog -fforce-recomp --make Test
./Test +RTS -N -ls -s