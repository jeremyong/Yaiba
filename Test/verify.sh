rm output
cd ..
cabal install --reinstall --ghc-options=-fforce-recomp -O2
cd -
ghc -fforce-recomp -rtsopts -fbreak-on-error -O2 -Odph -threaded --make Verify
./Verify +RTS -N -s > output
echo 'Running M2 script yaibaTest.m2 now. No output from here on out is win.'
M2 --script yaibaTest.m2