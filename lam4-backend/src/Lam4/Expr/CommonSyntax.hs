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

------ for record decl -----------------------------------------------

type RecordLabel = Text

data RecordDeclMetadata = RecordDeclMetadata
  { transparency :: Transparency
  , description  :: Maybe Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default RecordDeclMetadata)

newtype RowMetadata = MkRowMetadata { description :: Maybe Text }
  deriving newtype (Eq, Show, Ord)
  deriving stock Generic
  deriving (Mergeable, ExtractSym, EvalSym) via (Default RowMetadata)

-----------------------------------------------

-- | For Records -- and not RecordDecl!
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
  | Ge        -- ^ greater-than-or-equal
  | Eq        -- ^ equality (of Booleans, numbers or atoms)
  | Ne        -- ^ inequality (of Booleans, numbers or atoms)
  | StrAppend -- ^ Append two strings, arity 2
   deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default BinOp)

data TypeExpr
  = TyCustom  Name
    -- ^ aka "CustomType" from the Langium grammar
  | TyBuiltin TyBuiltin
    -- ^ aka "BuiltinType" from the Langium grammar
  | TyFun     TypeExpr TypeExpr
    -- ^ functions
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default TypeExpr)

data RowTypeDecl = MkRowTypeDecl { name      :: Name
                                 , typeAnnot :: TypeExpr
                                 , metadata  :: RowMetadata
                                 }
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default RowTypeDecl)

data TyBuiltin = BuiltinTypeString | BuiltinTypeInteger | BuiltinTypeBoolean
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default TyBuiltin)

-- TODO: Think about stuffing other metadata here too?
data DataDecl = RecordDecl [RowTypeDecl] [Name] RecordDeclMetadata -- ^ Labels Parents Description RecordDeclMetadata
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default DataDecl)

-- | Basically a 'Binding'
data DeclF expr =
    NonRec      Name expr
  | Rec         Name expr
  | Eval        expr
  | DataDecl    Name DataDecl
  deriving stock (Show, Eq, Ord, Generic)

makePrisms ''TypeExpr
makePrisms ''DataDecl
makePrisms ''DeclF
