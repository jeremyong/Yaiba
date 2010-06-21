cd ..
cabal install --reinstall --ghc-option=-auto-all
cd -
ghc -O2 -prof -auto-all -fforce-recomp --make Test
./Test +RTS -p