{-
TODO:
* Add Builtin list operations
-}

module Lam4.Expr.ConcreteSyntax where

import           Base
import           Lam4.Expr.Name (Name (..))

data Relatum
  = CustomType  Name
  | BuiltinType BuiltinTypeForRelation
  deriving stock (Eq, Show, Ord, Generic)

data BuiltinTypeForRelation = BuiltinTypeString | BuiltinTypeInteger | BuiltinTypeBoolean
  deriving stock (Eq, Show, Ord, Generic)

{- | References to where in the original source this corresponds to; e.g., §10.
      In the future, the structure will be more complicated --- want to be able to support things like `§10(1)(a)`
-}
newtype OriginalRuleRef
  = MkOriginalRuleRef Text
  deriving newtype (Eq, Ord)
  deriving stock (Show)

data Decl =
    NonRec Name Expr
  | Rec    Name Expr
  deriving stock Show

-- TODO: think more about Sigs!
data Expr
  = Var Name
  | Literal    Literal
  | Unary      UnaryOp Expr
  | BinExpr    BinOp Expr Expr
  | IfThenElse Expr Expr Expr
  -- | ListExpr   ListOp [Expr]
  | FunApp     Expr [Expr]
  | PredApp    Expr [Expr]
  | Fun        [Name] Expr (Maybe OriginalRuleRef) -- Function
  | Predicate  [Name] Expr (Maybe OriginalRuleRef) -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | Let        Name Expr Expr
  | Letrec     Name Expr Expr
  | Sig        [Name] [Expr]                       -- Sig parents relations
  | Relation   Name Name Relatum (Maybe Text)      -- Relation relName relParentSigName relatum description
  deriving stock (Eq, Show, Ord)

-- TODO: tweak the grammar to distinguish between integers and non-integers
data Literal
  = IntegerLiteral Integer
  | BooleanLiteral Bool
  | StringLiteral Text
  deriving stock (Eq, Show, Ord)

{-
TO ADD:
 Modulo / (integer) remainder
-}
data BinOp
  = Or
  | And
  | Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Lte
  | Gt
  | Gte
  | Equals
  | NotEquals
  | Join
   deriving stock (Eq, Show, Ord)

data UnaryOp = Not | UnaryMinus
  deriving stock (Eq, Show, Ord)

