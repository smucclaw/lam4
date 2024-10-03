{-# OPTIONS_GHC -Wno-orphans #-}

module Base.Aeson (module X) where

import           Base.Plated       as X (Plated, plate, cosmos)
import           Data.Aeson        as X
import           Data.Aeson.Encode.Pretty as X
import           Data.Aeson.Key    as X
import           Data.Aeson.KeyMap as X
import           Data.Aeson.Optics as X
import           Data.Aeson.Types  as X


instance Plated Value where
  plate f (Object o) = Object <$> Prelude.traverse f o
  plate f (Array a)  = Array <$> Prelude.traverse f a
  plate _ xs         = pure xs
  {-# INLINE plate #-}

