{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-================================================
          AST for concrete eval
================================================-}

module Lam4.Expr.ConEvalAST (
          ConEvalExpr(..), 
          ConEvalDecl, 
          DeclF(..)) 
  where

import           Base
import           Lam4.Expr.Name (Name (..))
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax (Lit (..))

type ConEvalDecl = DeclF ConEvalExpr

{- | AST for concrete-evaluation-*only* backends like Simala
Sep 2024: Normative constructs won't be added to AST till have wired to a basic evaluator
Other main differences between this and concrete syntax:
  * No @Predicate@, @PredApp@
  * No @Relation@
 -}
data ConEvalExpr
  = Var        Name
  | Lit        Lit
  | Cons       ConEvalExpr ConEvalExpr
  | List       [ConEvalExpr] 
  | Unary      UnaryOp ConEvalExpr
  | BinExpr    BinOp ConEvalExpr ConEvalExpr
  | IfThenElse ConEvalExpr ConEvalExpr ConEvalExpr
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- | ListExpr   ListOp [Union Expr]
  | FunApp     ConEvalExpr [ConEvalExpr]
  | Record     (Row ConEvalExpr)                          -- record construction
  | Project    ConEvalExpr Name                           -- record projection
  | Fun        RuleMetadata [Name] ConEvalExpr            -- Function
  | Let        ConEvalDecl ConEvalExpr
  | Foldl      ConEvalExpr ConEvalExpr ConEvalExpr        -- Foldl update  initial collection
  | Foldr      ConEvalExpr ConEvalExpr ConEvalExpr        -- Foldr combine nil     collection

  -- | StatementBlock  (NonEmpty Statement)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}
  | Atom        [Name]                                   -- Atom parents relations. This is what a ONE SIG with no relations will get translated to.
  deriving stock (Eq, Show, Generic)

makeLenses ''ConEvalExpr