rm output
cd ..
cabal install
cd -
ghc -fforce-recomp -Odph -threaded --make Verify
./Verify +RTS -N -s > output
echo 'Running M2 script yaibaTest.m2 now. No output from here on out is win.'
M2 --script yaibaTest.m2