module Name where

import qualified Data.Text as T

type Unique = Int
data Name = MkName { name :: T.Text, unique :: Unique }

instance Eq Name where
  (==) (MkName _ u) (MkName _ u') = u == u'

instance Ord Name where
  compare (MkName _ u) (MkName _ u') = compare u u'

instance Show Name where
  show (MkName n u) = T.unpack n <> "_" <> show u
