rm output
cd ..
#cabal install
cd -
ghc -O2 -threaded -eventlog --make Verify
./Verify +RTS -ls -N -s > output
echo 'Running M2 script yaibaTest.m2 now. No output from here on out is win.'
M2 --script yaibaTest.m2