cd ..
cabal install --reinstall --ghc-option=-auto-all --enable-library-profiling --enable-executable-profiling
cd -
ghc -O2 -prof -auto-all -fforce-recomp --make Verify
./Verify +RTS -p -h -s