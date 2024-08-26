module Lam4.Expr.AbstractSyntax where

import           Base
import           Base.Grisette

import           Lam4.Expr.Name (Name (..))
import           Lam4.Expr.CommonSyntax

data Expr
  = Var        Name
  | Lit        SymLit
  | Unary      UnaryOp (Union Expr)
  | BinExpr    BinOp (Union Expr) (Union Expr)
  | IfThenElse (Union Expr) (Union Expr) (Union Expr)
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- | ListExpr   ListOp [Union Expr]
  | FunApp     (Union Expr) [Union Expr]
  | Record     (Row (Union Expr))                          -- record construction
  | Project    (Union Expr) Name                           -- record projection
  | Fun        [Name] (Union Expr) (Maybe OriginalRuleRef) -- Function
  | Let        Name (Union Expr) (Union Expr)
  | Letrec     Name (Union Expr) (Union Expr)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}

  -- WIP deontics-related things
  -- | NormExpr   [Norm]

  | Predicate  [Name] (Union Expr) (Maybe OriginalRuleRef) -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | PredApp    (Union Expr) [Union Expr]
  -- no sig related constructs for now
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default Expr)


data SymLit
  = IntLit SymInteger
  | BoolLit SymBool
  -- | StringLit Text -- TODO: not clear that we need this
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Mergeable, SymEq) via (Default SymLit)