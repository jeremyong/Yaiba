#rm output
#cd ..
#cabal install --reinstall --ghc-options=-fforce-recomp -O2
#cd -
ghc -fforce-recomp -O2 -Odph -threaded --make Bench
./Bench +RTS -N #-t png:450x175 -k png:450x175