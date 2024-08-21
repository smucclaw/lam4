{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
The treatment of names is adapted from http://blog.vmchale.com/article/intern-identifiers
-}

module Lam4.Expr.Name (Name(..), Unique) where

import Base.Text qualified as T
import Base (makeFieldLabelsNoPrefix)

type Unique = Int

data Name = MkName
  { name :: T.Text
  , unique :: !Unique
  }
makeFieldLabelsNoPrefix ''Name

instance Eq Name where
  (==) (MkName _ u) (MkName _ u') = u == u'

instance Ord Name where
  compare (MkName _ u) (MkName _ u') = compare u u'

instance Show Name where
    show (MkName t u) = show t <> "_" <> show u
