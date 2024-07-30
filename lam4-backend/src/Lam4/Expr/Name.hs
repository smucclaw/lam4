{- |
The treatment of names is adapted from http://blog.vmchale.com/article/intern-identifiers
-}

module Lam4.Expr.Name (Name (..), Unique (..)) where

import Base.Text qualified as T

newtype Unique = Unique {unUnique :: Int}
  deriving newtype (Eq, Ord, Show)

data Name = Name
  { name :: T.Text
  , unique :: !Unique
  }

instance Eq Name where
  (==) (Name _ u) (Name _ u') = u == u'

instance Ord Name where
  compare (Name _ u) (Name _ u') = compare u u'

instance Show Name where
    show (Name t u) = show t <> "_" <> show u
