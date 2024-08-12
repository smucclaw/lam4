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


-- newtype Program = MkProgram [Expr]
--   deriving stock (Show)
--   deriving newtype (Eq, Ord)

-- TODO: think more about Sigs!
data Expr
  = Var Name
  | Literal    Literal
  | Unary      UnaryOp Expr
  | BinExpr    BinOp Expr Expr
  -- | ListExpr   ListOp [Expr]
  | FunApp     Expr [Expr]
  | PredApp    Expr [Expr]
  | Join       Expr Expr
  | Fun        [Name] Expr          -- Function
  | Predicate  [Name] Expr          -- Differs from a function when doing symbolic evaluation
  | Let        Name Expr Expr
  | Letrec     Name Expr Expr
  -- | SeqOfExpr  SeqExpr
  | Sig        [Name] [Expr]        -- Sig parents relations
  | Relation   Relatum (Maybe Text) -- Relation relatum description
  deriving stock (Eq, Show, Ord)

-- newtype SeqExpr = MkSeqExpr [Expr]
--   deriving stock (Show, Generic, Eq, Ord)
--   deriving (Semigroup, Monoid) via [Expr]

-- seqExpToExprs :: SeqExpr -> [Expr]
-- seqExpToExprs = coerce

-- exprsToSeqExp :: [Expr] -> SeqExpr
-- exprsToSeqExp = coerce

-- consSeqExpr :: Expr -> SeqExpr -> SeqExpr
-- consSeqExpr expr (MkSeqExpr exprs) = exprsToSeqExp $ expr : exprs

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
  | Lt
  | Lte
  | Gt
  | Gte
  | Equals
  | NotEquals
   deriving stock (Eq, Show, Ord)

data UnaryOp = Not | UnaryMinus
  deriving stock (Eq, Show, Ord)

