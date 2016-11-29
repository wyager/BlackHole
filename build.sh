export CABAL=/lusr/opt/ghc-7.10.3/bin/cabal
echo "Installing dependencies (may take a few minutes)"
$CABAL update
$CABAL sandbox init
$CABAL install --only-dependencies -j8
echo "Building executable"
$CABAL build
cp dist/build/BlackHole/BlackHole ./BlackHole