cd ..
cabal install --reinstall
cd -
ghc -threaded -Odph -eventlog -fforce-recomp --make Verify
./Verify +RTS -N7 -ls -s