{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lam4.Expr.SymEvalAST where

import           Base
import           Base.Grisette

import           Lam4.Expr.Name (Name (..))
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax qualified as CST (Lit (..), Decl)

{-================================================
          For symbolic eval
================================================-}

-- | For symbolic evaluation
newtype SymExpr = MkSymExpr (SymExprF (Union SymExpr))
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  deriving (Mergeable, ExtractSym, EvalSym) via (Default (SymExprF (Union SymExpr)))

{- | Sep 2024: Normative constructs won't be added to AST till have wired to a basic evaluator -}
data SymExprF a
  = Var        Name
  | Lit        SymLit
  | Cons       a a
  | List       [a] 
  | Unary      UnaryOp a
  | BinExpr    BinOp a a
  | IfThenElse a a a
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- | ListExpr   ListOp [Union Expr]
  | FunApp     a [a]
  | Record     (Row a)                          -- record construction
  | Project    a Name                           -- record projection
  | Fun        RuleMetadata [Name] a            -- Function
  | Let        SymDecl a
  -- | StatementBlock  (NonEmpty Statement)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}

  | Predicate  RuleMetadata [Name] a            -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | PredApp    a [a]
  | Sig        [Name] [a]                       -- Sig parents relations
  | Relation   Name Name TypeExpr (Maybe Text)  -- Relation relName relParentSigName relatum description
  deriving stock (Eq, Show, Generic)


newtype SymDecl = SymDecl (DeclF SymExpr)
  deriving newtype (Eq, Show)
  deriving Generic
  deriving (Mergeable, ExtractSym, EvalSym) via (Default (DeclF SymExpr))

data SymLit
  = IntLit SymInteger
  | BoolLit SymBool
  -- | StringLit Text -- TODO: not clear that we need this
  deriving stock (Eq, Show, Generic)
  deriving (Mergeable, EvalSym, ExtractSym, SymEq) via (Default SymLit)



makeLenses ''SymExpr