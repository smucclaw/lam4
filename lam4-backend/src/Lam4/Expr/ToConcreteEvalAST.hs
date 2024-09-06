module Lam4.Expr.ToConcreteEvalAST where

import           Base
import qualified Lam4.Expr.CEvalAST as AST
import           Lam4.Expr.ConcreteSyntax (exprSubexprs)
import qualified Lam4.Expr.ConcreteSyntax as CST


desugarForConcreteEval ::  CST.Expr -> CST.Expr
desugarForConcreteEval = transformOf exprSubexprs $ \case
  CST.Predicate ruleMetadata params body -> CST.Fun ruleMetadata params (desugarForConcreteEval body)
  CST.PredApp predicate args             -> CST.FunApp (desugarForConcreteEval predicate) (map desugarForConcreteEval args)
  x -> x 

toCEvalDecl :: CST.Decl -> AST.CEvalDecl
toCEvalDecl = \case
  CST.NonRec name expr       -> AST.NonRec name (toCEvalExpr expr)
  CST.Rec name expr          -> AST.Rec name (toCEvalExpr expr)
  CST.TypeDecl name typedecl -> AST.TypeDecl name typedecl

toCEvalExpr :: CST.Expr -> AST.CEvalExpr
toCEvalExpr expression = 
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
        CST.Let decl body                -> AST.Let (toCEvalDecl decl) (swapConstructors body)
        
        CST.Predicate{}                  -> error "CST.Predicate should have been desugared"
        CST.PredApp{}                    -> error "CST.PredApp should have been desugared"
        
        CST.Sig parents relations        -> AST.Sig parents (map swapConstructors relations)
        CST.Relation relName relParentSigName relatum description -> AST.Relation relName relParentSigName relatum description
        CST.StatementBlock{}             -> error "swapConstructors: StatementBlock not yet implemented"
        CST.NormIsInfringed{}            -> error "swapConstructors: NormIsInfringed not yet implemented"
