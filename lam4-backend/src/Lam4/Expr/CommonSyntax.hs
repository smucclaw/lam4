module Lam4.Expr.CommonSyntax where

import           Base
import           Base.Grisette
import           Lam4.Expr.Name   (Name (..))

type RecordLabel = Text

-- | Basically a 'Binding'
data DeclF expr typedecl =
    NonRec      Name expr
  | Rec         Name expr
  | TypeDecl    Name typedecl
  deriving stock (Show, Eq, Ord, Generic)

-- TODO: Think about stuffing other metadata here too?
data TypeDecl = RecordDecl [RowTypeDecl] [Name] (Maybe Text) -- ^ Labels Parents Description
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default TypeDecl)

type RowTypeDecl = (Name, TypeExpr)


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
  | Cons    -- ^ List Cons
   deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default BinOp)

data TypeExpr
  = CustomType  Name
  | BuiltinType BuiltinType
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default TypeExpr)

data BuiltinType = BuiltinTypeString | BuiltinTypeInteger | BuiltinTypeBoolean
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default BuiltinType)
