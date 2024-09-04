module Lam4.Expr.ToAST where

import           Lam4.Expr.AbstractSyntax (mkCEvalExpr)
import qualified Lam4.Expr.AbstractSyntax as AST
import qualified Lam4.Expr.ConcreteSyntax as CST

toCEvalAst :: CST.Expr -> AST.CEvalExpr
toCEvalAst = \case
  CST.Var name                -> mkCEvalExpr $ AST.Var name
  CST.Lit lit                 -> mkCEvalExpr $ AST.Lit lit
  CST.Unary op expr           -> mkCEvalExpr $ AST.Unary op (toCEvalAst expr)
  CST.BinExpr op left right   -> mkCEvalExpr $ AST.BinExpr op (toCEvalAst left) (toCEvalAst right)
  CST.IfThenElse cond thn els -> mkCEvalExpr $ AST.IfThenElse (toCEvalAst cond) (toCEvalAst thn) (toCEvalAst els)

  CST.FunApp fun args -> mkCEvalExpr $ AST.FunApp (toCEvalAst fun) (map toCEvalAst args)
  CST.Record rows       -> mkCEvalExpr $ AST.Record (map (fmap toCEvalAst) rows)
  CST.Project record label      -> mkCEvalExpr $ AST.Project (toCEvalAst record) label
  CST.Fun args body mOrigRuleRef   ->  mkCEvalExpr $  AST.Fun args (toCEvalAst body) mOrigRuleRef
  CST.Let decl body     -> mkCEvalExpr $ AST.Let decl (toCEvalAst body)
  CST.Predicate params body mOrigRuleRef -> mkCEvalExpr $ AST.Predicate params (toCEvalAst body) mOrigRuleRef
  CST.PredApp predicate args -> mkCEvalExpr $ AST.PredApp (toCEvalAst predicate) (map toCEvalAst args)
  CST.Sig parents relations -> mkCEvalExpr $ AST.Sig parents (map toCEvalAst relations)
  CST.Relation relName relParentSigName relatum description -> mkCEvalExpr $ AST.Relation relName relParentSigName relatum description

  CST.List           _  -> error "toCEvalAst: List not yet implemented"
  CST.StatementBlock _  -> error "toCEvalAst: StatementBlock not yet implemented"
  CST.NormIsInfringed _ -> error "toCEvalAst: NormIsInfringed not yet implemented"
  -- CST.Cons left right -> mkCEvalExpr $ AST.Cons (toCEvalAst left) (toCEvalAst right)
