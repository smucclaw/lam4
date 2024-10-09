#!/bin/bash

set -euo pipefail

gf -make -f haskell --haskell=gadt Lam4Eng.gf
cat Lam4.hs |
    sed 's/module Lam4 where/module Lam4.Render.Lam4Gf where/' | \
    sed 's/instance Show .*//' | \
    sed 's/-- below this line machine-generated/-- below this line machine-generated\ninstance (Gf (Tree a)) => Show (Tree a) where\n    show = showExpr [] . gf/' | \
    sed 's/LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances/LANGUAGE GADTs, UndecidableInstances/' | \
    sed 's/import Control.Monad.Identity/import Control.Monad.Identity (Identity ( Identity, runIdentity))/' | \
    sed 's/import Data.Monoid/import Control.Monad (MonadPlus, ap, mzero, mplus)/' | \
    sed 's/import PGF hiding (Tree)/import PGF (Expr, mkApp, mkCId, mkFloat, mkInt, mkStr, showCId, showExpr, unApp, unFloat, unInt, unStr )/'  > ../src/Lam4/Render/Lam4Gf.hs

echo "moved Lam4.hs into ../src/Lam4/Render/Lam4Gf.hs"
# head -5 ../src/LS/NLP/NL4.hs
rm Lam4.hs
