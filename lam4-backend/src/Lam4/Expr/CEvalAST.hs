{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-================================================
          AST for concrete eval
================================================-}

module Lam4.Expr.CEvalAST (
          CEvalExpr(..), 
          CEvalDecl, 
          DeclF(..)) 
  where

import           Base
import           Lam4.Expr.Name (Name (..))
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax (Lit (..))

type CEvalDecl = DeclF CEvalExpr

{- | AST for concrete evaluation
Sep 2024: Normative constructs won't be added to AST till have wired to a basic evaluator
Other main differences between this and concrete syntax:
  * No @Predicate@, @PredApp@
 -}
data CEvalExpr
  = Var        Name
  | Lit        Lit
  | Cons       CEvalExpr CEvalExpr
  | List       [CEvalExpr] 
  | Unary      UnaryOp CEvalExpr
  | BinExpr    BinOp CEvalExpr CEvalExpr
  | IfThenElse CEvalExpr CEvalExpr CEvalExpr
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- | ListExpr   ListOp [Union Expr]
  | FunApp     CEvalExpr [CEvalExpr]
  | Record     (Row CEvalExpr)                          -- record construction
  | Project    CEvalExpr Name                           -- record projection
  | Fun        RuleMetadata [Name] CEvalExpr            -- Function
  | Let        CEvalDecl CEvalExpr
  -- | StatementBlock  (NonEmpty Statement)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}
  | Sig        [Name] [CEvalExpr]                       -- Sig parents relations
  | Relation   Name Name TypeExpr (Maybe Text)  -- Relation relName relParentSigName relatum description
  deriving stock (Eq, Show, Generic)

makeLenses ''CEvalExpr