{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
The treatment of names is adapted from http://blog.vmchale.com/article/intern-identifiers
-}

module Lam4.Expr.Name (Name(..), Unique, NodeNameStatus(..)) where

import           Base          (Generic, makeFieldLabelsNoPrefix)
import           Base.Grisette
import qualified Base.Text     as T

type Unique = Int

-- | For, e.g., downstream post-processing of names
data NodeNameStatus = IsEntrypoint | NotEntrypoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default NodeNameStatus    )

data Name = MkName
  { name               :: T.Text
  , unique             :: !(Maybe Unique)
  , referentNodeStatus :: !NodeNameStatus
  -- TODO: Might be better to stash entrypoint info elsewhere
  }
  deriving stock Generic
  deriving (Mergeable, ExtractSym, EvalSym) via (Default Name)
makeFieldLabelsNoPrefix ''Name


instance Eq Name where
  (==) (MkName _ u _) (MkName _ u' _) = u == u'

instance Ord Name where
  compare (MkName _ u _) (MkName _ u' _) = compare u u'

instance Show Name where
    show (MkName t u langiumNodeType) = show t <> "_" <> show u <> "_" <> show langiumNodeType
