#!/bin/sh

./.cabal-sandbox/bin/cauterize-test \
  crucible --build-cmd="../../.cabal-sandbox/bin/caut-ghc7-ref --spec=%s --meta=%m --output=hs" \
           --build-cmd="cd hs && cabal sandbox init --sandbox=../../../.cabal-sandbox" \
           --build-cmd="cd hs && cabal install --only-dependencies" \
           --build-cmd="cd hs && cabal build" \
           --run-cmd="./hs/dist/build/*_crucible_client/*_crucible_client" \
           --schema-count=1 \
           --instance-count=200 \
           --type-count=10 \
           --enc-size=1024
