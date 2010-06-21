cd ..
cabal install --reinstall -w /home/jeremyong/Software/ghc613/bin/ghc -O --ghc-options=-fllvm
cd -
/home/jeremyong/Software/ghc613/bin/ghc -threaded -O2 -eventlog -fllvm -fforce-recomp -rtsopts --make Test
./Test +RTS -N -ls -s