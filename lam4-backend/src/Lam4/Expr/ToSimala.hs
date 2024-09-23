{-# LANGUAGE DataKinds #-}

module Lam4.Expr.ToSimala (
    -- * Entry point
    compile

    -- * Compile Helpers
  , compileDecl
  , compileExpr

    -- * Utilities to work with simala terms (basically a copy and paste)
  , render

    -- * Convenient type aliases
  , SimalaProgram
  , SimalaDecl

  -- re-exports
  , SM.doEvalDeclsTracing
  , SM.TraceMode(..)
  , SM.emptyEnv
) where

import           Base
import qualified Base.Text                as T
import           Lam4.Expr.CommonSyntax   (BinOp (..), RuleMetadata (..),
                                           Transparency (..), UnaryOp (..))
import           Lam4.Expr.ConcreteSyntax as CST (Lit (..))
import           Lam4.Expr.ConEvalAST     as AST
import           Lam4.Expr.Name           (Name (..), uniqueForNamesThatShouldNotHaveUniqueAppended)
-- import qualified Simala.Expr.Parser       as SM
import           Data.Bifunctor           (bimap)
import qualified Simala.Expr.Evaluator    as SM (doEvalDeclsTracing)
import qualified Simala.Expr.Render       as SM
import qualified Simala.Expr.Type         as SM


defaultTransparency :: SM.Transparency
defaultTransparency = SM.Opaque

simalaNegativeOneInt :: SM.Expr
simalaNegativeOneInt = SM.Lit $ SM.IntLit (-1)

simalaNegativeOneFrac :: SM.Expr
simalaNegativeOneFrac = SM.Lit $ SM.FracLit (-1.0)

type SimalaDecl = SM.Decl
type SimalaProgram = [SM.Decl]

lam4ToSimalaName :: Name -> SM.Name
lam4ToSimalaName (MkName name unique) =
  if unique == uniqueForNamesThatShouldNotHaveUniqueAppended
  then name
  else name <> "_" <> T.pack (show unique)

compileTransparency :: Transparency -> SM.Transparency
compileTransparency = \case
  Opaque      -> SM.Opaque
  Transparent -> SM.Transparent

compileUnaryOp :: UnaryOp -> SM.Builtin
compileUnaryOp = \case
  UnaryMinus        -> SM.Minus
  Not               -> SM.Not
  Floor             -> SM.Floor
  Ceiling           -> SM.Ceiling
  IntegerToFraction -> SM.FromInt

-- TODO: May want to change Plus and Mult to Sum and Product
compileBinOp :: BinOp -> SM.Builtin
compileBinOp = \case
  Plus      -> SM.Sum
  Minus     -> SM.Minus
  And       -> SM.And
  Or        -> SM.Or
  Mult      -> SM.Product
  Divide    -> SM.Divide
  Lt        -> SM.Lt
  Gt        -> SM.Gt
  Le        -> SM.Le
  Ge        -> SM.Ge
  Ne        -> SM.Ne
  Eq        -> SM.HEq
  -- TODO: in the future, could pass along type info and use that to determine if we should use HEq or Eq
  Modulo    -> SM.Modulo
  StrAppend -> SM.Append


compile  :: [AST.ConEvalDecl] -> SimalaProgram
compile decls = decls
  & filter (\case { DataDecl{} -> False; _ -> True })
  & map compileDecl

compileDecl :: AST.ConEvalDecl -> SimalaDecl
compileDecl = \case

  ------------------------------------
  ---- EXPERIMENTAL -----------------
  ------------------------------------
  NonRec name Atom {} ->
    let smName = lam4ToSimalaName name
    in SM.NonRec defaultTransparency smName (SM.Atom $ SM.MkAtom smName)
  -- TODO: May not want to translate ONE SIG with no relations this way
  -- TODO: Not sure if naming is ok

  ------------------------------------
  ---- VANILLA -----------------------
  ------------------------------------
  NonRec name fun@(Fun ruleMetadata _ _) -> SM.NonRec (compileTransparency ruleMetadata.transparency) (lam4ToSimalaName name) (compileExpr fun)
  Rec name fun@(Fun ruleMetadata _ _)    -> SM.Rec (compileTransparency ruleMetadata.transparency) (lam4ToSimalaName name) (compileExpr fun)

  NonRec name expr -> SM.NonRec defaultTransparency (lam4ToSimalaName name) (compileExpr expr)
  Rec name expr    -> SM.Rec defaultTransparency (lam4ToSimalaName name) (compileExpr expr)
  Eval expr        -> SM.Eval (compileExpr expr)
  -- TODO: Improve the error handling later
  DataDecl{}       -> error "[ToSimala] Type declarations not supported in Simala and should already have been pre-filtered out, prior to this phase"


compileExpr :: AST.ConEvalExpr -> SM.Expr
compileExpr = \case
  Var name                     -> SM.Var $ lam4ToSimalaName name
  Lit (CST.IntLit i)           -> SM.Lit $ SM.IntLit i
  Lit (CST.FracLit fractional) -> SM.Lit $ SM.FracLit fractional
  Lit (CST.BoolLit b)          -> SM.Lit $ SM.BoolLit b
  Lit (CST.StringLit s)        -> SM.Lit $ SM.StringLit s
  Cons first rest              -> SM.Cons (compileExpr first) (compileExpr rest)
  List xs                      -> SM.List (map compileExpr xs)

  -- translate unary minus x to @-1 * x@, accounting for Int vs Frac literals
  -- This obviously won't be enough for applications of UnaryMinus to a non-literal (since can't tell if it's an Int or Frac)
  Unary UnaryMinus lit@(Lit (CST.IntLit _)) -> SM.Builtin SM.Product [simalaNegativeOneInt, compileExpr lit]
  Unary UnaryMinus lit@(Lit (CST.FracLit _)) -> SM.Builtin SM.Product [simalaNegativeOneFrac, compileExpr lit]
  Unary op expr                -> SM.Builtin (compileUnaryOp op) [compileExpr expr]
  BinExpr op left right        -> SM.Builtin (compileBinOp op) [compileExpr left, compileExpr right]
  IfThenElse cond thn els      -> SM.Builtin SM.IfThenElse [compileExpr cond, compileExpr thn, compileExpr els]
  FunApp fun args              -> SM.App (compileExpr fun) (map compileExpr args)
  Record rows                  -> SM.Record $ map (bimap lam4ToSimalaName compileExpr) rows
  Project record label         -> SM.Project (compileExpr record) (lam4ToSimalaName label)
  Fun ruleMetadata params body -> SM.Fun (compileTransparency ruleMetadata.transparency) (map lam4ToSimalaName params) (compileExpr body)
  Let decl body                -> SM.Let (compileDecl decl) (compileExpr body)
  Atom{}                        -> error "Should already have translated a ONE CONCEPT / SIG with no Relations to a Simala Atom when compiling decls"
  Foldr combine nil xs         -> SM.Builtin SM.Foldr [compileExpr combine, compileExpr nil, compileExpr xs]
  Foldl update initial xs      -> SM.Builtin SM.Foldl [compileExpr update, compileExpr initial, compileExpr xs]

-------------------------

-- | Copied from https://github.com/smucclaw/dsl/blob/e77def08949cac1af094dfdb828c35833c36b8b7/lib/haskell/natural4/src/LS/XPile/Simala/Transpile.hs
render :: [SM.Decl] -> Text
render = T.unlines . fmap ((<> simalaLineDelimiter) . SM.render)
  where
    simalaLineDelimiter = " ;"
