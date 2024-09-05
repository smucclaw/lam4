module Lam4.Expr.ToAST where

-- import           Base
import           Lam4.Expr.AbstractSyntax (mkCEvalExpr)
import qualified Lam4.Expr.AbstractSyntax as AST
-- import           Lam4.Expr.ConcreteSyntax (exprSubexprs)
import qualified Lam4.Expr.ConcreteSyntax as CST


toCEvalAST :: CST.Expr -> AST.CEvalExpr
toCEvalAST = \case
  CST.Var name                     -> mkCEvalExpr $ AST.Var name
  CST.Lit lit                      -> mkCEvalExpr $ AST.Lit lit
  CST.Cons first rest              -> mkCEvalExpr $ AST.Cons (toCEvalAST first) (toCEvalAST rest)
  CST.List xs                      -> mkCEvalExpr $ AST.List (map toCEvalAST xs)

  CST.Unary op expr                -> mkCEvalExpr $ AST.Unary op (toCEvalAST expr)
  CST.BinExpr op left right        -> mkCEvalExpr $ AST.BinExpr op (toCEvalAST left) (toCEvalAST right)
  CST.IfThenElse cond thn els      -> mkCEvalExpr $ AST.IfThenElse (toCEvalAST cond) (toCEvalAST thn) (toCEvalAST els)

  CST.FunApp fun args              -> mkCEvalExpr $ AST.FunApp (toCEvalAST fun) (map toCEvalAST args)
  CST.Record rows                  -> mkCEvalExpr $ AST.Record (map (fmap toCEvalAST) rows)
  CST.Project record label         -> mkCEvalExpr $ AST.Project (toCEvalAST record) label
  CST.Fun ruleMetadata args body   -> mkCEvalExpr $ AST.Fun ruleMetadata args (toCEvalAST body)
  CST.Let decl body                -> mkCEvalExpr $ AST.Let decl (toCEvalAST body)
  CST.Predicate ruleMetadata params body -> mkCEvalExpr $ AST.Predicate ruleMetadata params (toCEvalAST body)
  CST.PredApp predicate args       -> mkCEvalExpr $ AST.PredApp (toCEvalAST predicate) (map toCEvalAST args)
  CST.Sig parents relations        -> mkCEvalExpr $ AST.Sig parents (map toCEvalAST relations)
  CST.Relation relName relParentSigName relatum description -> mkCEvalExpr $ AST.Relation relName relParentSigName relatum description
  CST.StatementBlock _             -> error "toCEvalAST: StatementBlock not yet implemented"
  CST.NormIsInfringed _            -> error "toCEvalAST: NormIsInfringed not yet implemented"
