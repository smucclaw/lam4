module Lam4.Expr.ToConcreteEvalAST (
  -- * entrypoint
  cstProgramToConEvalProgram,
  -- * helpers
  toConEvalDecl,
  toConEvalExpr)
where

import           Base
import           Lam4.Expr.ConcreteSyntax (exprSubexprs)
import qualified Lam4.Expr.ConcreteSyntax as CST
import qualified Lam4.Expr.ConEvalAST     as AST

-- | Entry point
cstProgramToConEvalProgram :: [CST.Decl] -> [AST.ConEvalDecl]
cstProgramToConEvalProgram = map toConEvalDecl


desugarForConcreteEval ::  CST.Expr -> CST.Expr
desugarForConcreteEval = transformOf exprSubexprs $ \case
  CST.Predicate ruleMetadata params body -> CST.Fun ruleMetadata params (desugarForConcreteEval body)
  CST.PredApp predicate args             -> CST.FunApp (desugarForConcreteEval predicate) (map desugarForConcreteEval args)
  x -> x

toConEvalDecl :: CST.Decl -> AST.ConEvalDecl
toConEvalDecl = \case
  CST.NonRec name expr       -> AST.NonRec name (toConEvalExpr expr)
  CST.Rec name expr          -> AST.Rec name (toConEvalExpr expr)
  CST.Eval expr              -> AST.Eval (toConEvalExpr expr)
  CST.TypeDecl name typedecl -> AST.TypeDecl name typedecl

toConEvalExpr :: CST.Expr -> AST.ConEvalExpr
toConEvalExpr expression =
  expression
  & desugarForConcreteEval
  & swapConstructors
    where
      swapConstructors = \case
        CST.Var name                     -> AST.Var name
        CST.Lit lit                      -> AST.Lit lit
        CST.Cons first rest              -> AST.Cons (swapConstructors first) (swapConstructors rest)
        CST.List xs                      -> AST.List (map swapConstructors xs)

        CST.Unary op expr                -> AST.Unary op (swapConstructors expr)
        CST.BinExpr op left right        -> AST.BinExpr op (swapConstructors left) (swapConstructors right)
        CST.IfThenElse cond thn els      -> AST.IfThenElse (swapConstructors cond) (swapConstructors thn) (swapConstructors els)

        CST.FunApp fun args              -> AST.FunApp (swapConstructors fun) (map swapConstructors args)
        CST.Record rows                  -> AST.Record (map (fmap swapConstructors) rows)
        CST.Project record label         -> AST.Project (swapConstructors record) label
        CST.Fun ruleMetadata params body -> AST.Fun ruleMetadata params (swapConstructors body)
        CST.Let decl body                -> AST.Let (toConEvalDecl decl) (swapConstructors body)

        CST.Predicate{}                  -> error "CST.Predicate should have been desugared"
        CST.PredApp{}                    -> error "CST.PredApp should have been desugared"

        CST.Relation{}                   -> error "swapConstructors: Relations not supported in ConcreteEvalOnly AST"
        CST.StatementBlock{}             -> error "swapConstructors: StatementBlock not yet implemented"
        CST.NormIsInfringed{}            -> error "swapConstructors: NormIsInfringed not yet implemented"

        ------------------------------------
        ----- EXPERIMENTAL -----------------
        ------------------------------------
        CST.Sig parents []               -> AST.Atom parents
        CST.Sig _       _                -> error "swapConstructors: A Sig with relations is not supported for concrete-evaluation-only backends"


