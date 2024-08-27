{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
The treatment of names is adapted from http://blog.vmchale.com/article/intern-identifiers
-}

module Lam4.Expr.Name (Name(..), Unique) where

import           Base          (Generic, makeFieldLabelsNoPrefix)
import           Base.Grisette
import qualified Base.Text     as T

type Unique = Int

data Name = MkName
  { name   :: T.Text
  , unique :: !Unique
  }
  deriving stock Generic
  deriving (Mergeable, ExtractSym, EvalSym) via (Default Name)
makeFieldLabelsNoPrefix ''Name


instance Eq Name where
  (==) (MkName _ u) (MkName _ u') = u == u'

instance Ord Name where
  compare (MkName _ u) (MkName _ u') = compare u u'

instance Show Name where
    show (MkName t u) = show t <> "_" <> show u
