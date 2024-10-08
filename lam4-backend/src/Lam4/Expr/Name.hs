{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
The treatment of names is adapted from http://blog.vmchale.com/article/intern-identifiers
-}

module Lam4.Expr.Name (Name(..), Unique, ReferentStatus(..)) where

import           Base          (Generic, makeFieldLabelsNoPrefix)
import           Base.Grisette
import qualified Base.Text     as T

type Unique = Int

-- | For, e.g., downstream post-processing of names
data ReferentStatus = IsEntrypoint | NotEntrypoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default ReferentStatus)

data Name = MkName
  { name               :: T.Text
  , unique             :: !(Maybe Unique)
  , referentNodeStatus :: !ReferentStatus
  -- TODO: Might be better to stash entrypoint info elsewhere
  }
  deriving stock Generic
  deriving (Mergeable, ExtractSym, EvalSym) via (Default Name)
makeFieldLabelsNoPrefix ''Name


instance Eq Name where
  (==) (MkName _ u _) (MkName _ u' _) = u == u'

instance Ord Name where
  compare (MkName _ (Just u) _) (MkName _ (Just u') _) = compare u u'
  compare (MkName _ Nothing _) (MkName _ Nothing _)    = EQ
  compare (MkName _ (Just _) _) (MkName _ Nothing _)   = GT
  compare (MkName _ Nothing _) (MkName _ (Just _) _)   = LT

instance Show Name where
    show (MkName t (Just u) langiumNodeType) = show t <> "_" <> show u <> "_" <> show langiumNodeType
    show (MkName t Nothing langiumNodeType) = show t <> show langiumNodeType
