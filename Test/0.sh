cd ..
cabal install --reinstall --ghc-options=-fforce-recomp
cd -
ghc -threaded -Odph -eventlog -fforce-recomp --make Verify
./Verify +RTS -N -ls -s