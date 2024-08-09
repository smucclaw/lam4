module Base.Plated (cosmos) where

import           Base
import           Control.Lens.Plated (Plated, plate)

cosmos :: Plated a => Fold a a
cosmos = cosmosOf (traversalVL plate)
