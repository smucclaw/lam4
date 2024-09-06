{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lam4.Expr.CommonSyntax where

import           Base
import           Base.Grisette
import           Lam4.Expr.Name (Name (..))


data Transparency =
    Opaque
  | Transparent
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default Transparency)

type RecordLabel = Text

data RecordDeclMetadata = RecordDeclMetadata
  { transparency :: Transparency
  , description  :: Maybe Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default RecordDeclMetadata)
makeFieldLabelsNoPrefix ''RecordDeclMetadata

type Row a = [(Name, a)]

{- | References to where in the original source this corresponds to; e.g., §10.
     WIP -- In the future, the structure will be more complicated.
     Want to be able to support things like `§10(1)(a)`
-}
newtype OriginalRuleRef
  = MkOriginalRuleRef Text
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default OriginalRuleRef)

-- | Metadata for something that's a FunDecl or PredicateDecl
data RuleMetadata = RuleMetadata
  { originalRuleRef :: Maybe OriginalRuleRef
  , transparency    :: Transparency
  , description     :: Maybe Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default RuleMetadata)
makeFieldLabelsNoPrefix ''RuleMetadata

emptyRuleMetadata :: RuleMetadata
emptyRuleMetadata = RuleMetadata Nothing Transparent Nothing

data UnaryOp = Not | UnaryMinus
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default UnaryOp)

-- | Binary operators. May want to distinguish between concrete and abstract versions in the future, but for now, the CST and AST use the same set of BinOps.
data BinOp
  = Or
  | And
  | Plus
  | Minus
  | Modulo   -- ^ (integer) remainder
  | Mult
  | Divide
  | Lt
  | Le
  | Gt
  | Ge      -- ^ greater-than-or-equal
  | Eq      -- ^ equality (of Booleans, numbers or atoms)
  | Ne      -- ^ inequality (of Booleans, numbers or atoms)
   deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default BinOp)

data TypeExpr
  = CustomType  Name
  | BuiltinType BuiltinType
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default TypeExpr)

type RowTypeDecl = (Name, TypeExpr)

data BuiltinType = BuiltinTypeString | BuiltinTypeInteger | BuiltinTypeBoolean
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default BuiltinType)

-- TODO: Think about stuffing other metadata here too?
data TypeDecl = RecordDecl [RowTypeDecl] [Name] RecordDeclMetadata -- ^ Labels Parents Description RecordDeclMetadata
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default TypeDecl)

-- | Basically a 'Binding'
data DeclF expr =
    NonRec      Name expr
  | Rec         Name expr
  | TypeDecl    Name TypeDecl
  deriving stock (Show, Eq, Ord, Generic)

makePrisms ''TypeExpr
makePrisms ''TypeDecl
makePrisms ''DeclF