{-# LANGUAGE DataKinds #-}

module Lam4.Expr.ToSimala (
    -- * Entry point
    compile

    -- * Helpers
  , compileDecl
  , compileExpr

    -- * Utilities to work with simala terms
  , render
) where

import           Base
import qualified Base.Text                as T
import           Lam4.Expr.ConEvalAST     as AST
import           Lam4.Expr.CommonSyntax   (BinOp (..), RuleMetadata (..),
                                           Transparency (..), UnaryOp (..))
import           Lam4.Expr.ConcreteSyntax as CST (Lit (..))
import           Lam4.Expr.Name           (Name (..))
-- import qualified Simala.Expr.Parser       as SM
import           Data.Bifunctor           (bimap)
import qualified Simala.Expr.Render       as SM
import qualified Simala.Expr.Type         as SM

defaultTransparency = SM.Transparent

lam4ToSimalaName :: Name -> SM.Name
lam4ToSimalaName (MkName name unique) = name <> "_" <> T.pack (show unique)

compileTransparency :: Transparency -> SM.Transparency
compileTransparency = \case
  Opaque      -> SM.Opaque
  Transparent -> SM.Transparent

compileUnaryOp :: UnaryOp -> SM.Builtin
compileUnaryOp = \case
  UnaryMinus -> SM.Minus
  Not        -> SM.Not

-- TODO: May want to change Plus and Mult to Sum and Product
compileBinOp :: BinOp -> SM.Builtin
compileBinOp = \case
  Plus   -> SM.Sum
  Minus  -> SM.Minus
  And    -> SM.And
  Or     -> SM.Or
  Mult   -> SM.Product
  Divide -> SM.Divide
  Lt     -> SM.Lt
  Gt     -> SM.Gt
  Le     -> SM.Le
  Ge     -> SM.Ge
  Ne     -> SM.Ne
  Eq     -> SM.Eq
  Modulo -> SM.Modulo


compile  :: [AST.CEvalDecl] -> [SM.Decl]
compile decls = decls
  & filter (\case { TypeDecl{} -> False; _ -> True })
  & map compileDecl

compileDecl :: AST.CEvalDecl -> SM.Decl
compileDecl = \case
  NonRec name Sig {} ->
    let smName = lam4ToSimalaName name
    in SM.NonRec defaultTransparency smName (SM.Atom smName)
  -- TODO: May not want to translate ONE SIG this way
  -- Also, may be problematic if the name for the Decl is the same as that for the atom

  NonRec name fun@(Fun ruleMetadata _ _) -> SM.NonRec (compileTransparency ruleMetadata.transparency) (lam4ToSimalaName name) (compileExpr fun)
  Rec name fun@(Fun ruleMetadata _ _)    -> SM.Rec (compileTransparency ruleMetadata.transparency) (lam4ToSimalaName name) (compileExpr fun)

  NonRec name expr -> SM.NonRec SM.Transparent (lam4ToSimalaName name) (compileExpr expr)
  Rec name expr    -> SM.Rec SM.Transparent (lam4ToSimalaName name) (compileExpr expr)
  -- TODO: Improve the error handling later
  TypeDecl{}       -> error "Type declarations not supported in Simala"


compileExpr :: AST.CEvalExpr -> SM.Expr
compileExpr = \case
  Var name                     -> SM.Var $ lam4ToSimalaName name
  Lit (CST.IntLit i)           -> SM.Lit $ SM.IntLit i
  Lit (CST.BoolLit b)          -> SM.Lit $ SM.BoolLit b
  Cons first rest              -> SM.Cons (compileExpr first) (compileExpr rest)
  List xs                      -> SM.List (map compileExpr xs)
  Unary op expr                -> SM.Builtin (compileUnaryOp op) [compileExpr expr]
  BinExpr op left right        -> SM.Builtin (compileBinOp op) [compileExpr left, compileExpr right]
  IfThenElse cond thn els      -> SM.Builtin SM.IfThenElse [compileExpr cond, compileExpr thn, compileExpr els]
  FunApp fun args              -> SM.App (compileExpr fun) (map compileExpr args)
  Record rows                  -> SM.Record $ map (bimap lam4ToSimalaName compileExpr) rows
  Project record label         -> SM.Project (compileExpr record) (lam4ToSimalaName label)
  Fun ruleMetadata params body -> SM.Fun (compileTransparency ruleMetadata.transparency) (map lam4ToSimalaName params) (compileExpr body)
  Let decl body                -> SM.Let (compileDecl decl) (compileExpr body)
  Sig{}                        -> error "Should already have translated ONE CONCEPT / SIG to a Simala Atom when compiling decls"
  Relation{}                   -> error "Relations not supported in Simala"

-------------------------

-- | Copied from https://github.com/smucclaw/dsl/blob/e77def08949cac1af094dfdb828c35833c36b8b7/lib/haskell/natural4/src/LS/XPile/Simala/Transpile.hs
render :: [SM.Decl] -> Text
render = T.unlines . fmap SM.render
