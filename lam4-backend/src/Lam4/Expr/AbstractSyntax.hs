module Lam4.Expr.AbstractSyntax where

import           Base
import           Base.Grisette

import           Lam4.Expr.Name (Name (..))
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax qualified as CST (Lit (..), Decl)


mkCEvalExpr :: ExprF CEvalExpr CST.Lit TypeExpr CST.Decl -> CEvalExpr
mkCEvalExpr = coerce

-- | For wiring up to a non-symbolic evaluator like Simala
newtype CEvalExpr = CEvalExpr (ExprF CEvalExpr CST.Lit TypeExpr CST.Decl)
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  
-- | For symbolic evaluation
newtype SymExpr = MkSymExpr (ExprF (Union SymExpr) SymLit TypeExpr Decl)
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default (ExprF (Union SymExpr) SymLit TypeExpr Decl))


newtype Decl = MyDecl (DeclF SymExpr TypeDecl)
  deriving newtype (Eq, Show)
  deriving Generic
  deriving (Mergeable, ExtractSym, EvalSym) via (Default (DeclF SymExpr TypeDecl))

{- | Sep 2024: Normative constructs won't be added to AST till have wired to a basic evaluator -}
data ExprF a lit typeE decl
  = Var        Name
  | Lit        lit
  | Cons       a a
  | Unary      UnaryOp a
  | BinExpr    BinOp a a
  | IfThenElse a a a
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- | ListExpr   ListOp [Union Expr]
  | FunApp     a [a]
  | Record     (Row a)                          -- record construction
  | Project    a Name                           -- record projection
  | Fun        [Name] a (Maybe OriginalRuleRef) -- Function
  | Let        decl a
  -- | StatementBlock  (NonEmpty Statement)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}

  | Predicate  [Name] a (Maybe OriginalRuleRef) -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | PredApp    a [a]
  | Sig        [Name] [a]                       -- Sig parents relations
  | Relation   Name Name typeE (Maybe Text)     -- Relation relName relParentSigName relatum description
  deriving stock (Eq, Show, Generic)

data SymLit
  = IntLit SymInteger
  | BoolLit SymBool
  -- | StringLit Text -- TODO: not clear that we need this
  deriving stock (Eq, Show, Generic)
  deriving (Mergeable, EvalSym, ExtractSym, SymEq) via (Default SymLit)
