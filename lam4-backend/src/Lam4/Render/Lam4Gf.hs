{-# LANGUAGE GADTs, UndecidableInstances #-}
module Lam4.Render.Lam4Gf where

import Control.Monad.Identity (Identity ( Identity, runIdentity))
import Control.Monad (MonadPlus, ap, mzero, mplus)
import PGF (Expr, mkApp, mkCId, mkFloat, mkInt, mkStr, showCId, showExpr, unApp, unFloat, unInt, unStr )

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
instance (Gf (Tree a)) => Show (Tree a) where
    show = showExpr [] . gf
----------------------------------------------------

type GBinOp = Tree GBinOp_
data GBinOp_
type GExpr = Tree GExpr_
data GExpr_
type GListExpr = Tree GListExpr_
data GListExpr_
type GListName = Tree GListName_
data GListName_
type GListRowTypeDecl = Tree GListRowTypeDecl_
data GListRowTypeDecl_
type GMetadata = Tree GMetadata_
data GMetadata_
type GName = Tree GName_
data GName_
type GRowTypeDecl = Tree GRowTypeDecl_
data GRowTypeDecl_
type GS = Tree GS_
data GS_
type GTypeDecl = Tree GTypeDecl_
data GTypeDecl_
type GUnaryOp = Tree GUnaryOp_
data GUnaryOp_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GAnd :: Tree GBinOp_
  GDivide :: Tree GBinOp_
  GEq :: Tree GBinOp_
  GGe :: Tree GBinOp_
  GGt :: Tree GBinOp_
  GLe :: Tree GBinOp_
  GLt :: Tree GBinOp_
  GMinus :: Tree GBinOp_
  GModulo :: Tree GBinOp_
  GMult :: Tree GBinOp_
  GNe :: Tree GBinOp_
  GOr :: Tree GBinOp_
  GPlus :: Tree GBinOp_
  GBinExpr :: GBinOp -> GExpr -> GExpr -> Tree GExpr_
  GFun :: GName -> GMetadata -> GListName -> GExpr -> Tree GExpr_
  GFunApp :: GExpr -> GListExpr -> Tree GExpr_
  GIfThenElse :: GExpr -> GExpr -> GExpr -> Tree GExpr_
  GNormIsInfringed :: GName -> Tree GExpr_
  GPredApp :: GExpr -> GListExpr -> Tree GExpr_
  GPredicate :: GName -> GMetadata -> GListName -> GExpr -> Tree GExpr_
  GProject :: GExpr -> GName -> Tree GExpr_
  GUnary :: GUnaryOp -> GExpr -> Tree GExpr_
  GVar :: GName -> Tree GExpr_
  GListExpr :: [GExpr] -> Tree GListExpr_
  GListName :: [GName] -> Tree GListName_
  GListRowTypeDecl :: [GRowTypeDecl] -> Tree GListRowTypeDecl_
  GMkMetadata :: GString -> Tree GMetadata_
  GNoMetadata :: Tree GMetadata_
  GMkName :: GString -> Tree GName_
  GMkRowDecl :: GMetadata -> GName -> Tree GRowTypeDecl_
  GMkRowTypeDecl :: GMetadata -> GName -> GName -> Tree GRowTypeDecl_
  GEmptyS :: Tree GS_
  GExprS :: GExpr -> Tree GS_
  GTypeDeclS :: GTypeDecl -> Tree GS_
  GMkTypeDecl :: GMetadata -> GName -> GListRowTypeDecl -> Tree GTypeDecl_
  GNot :: Tree GUnaryOp_
  GUnaryMinus :: Tree GUnaryOp_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GAnd,GAnd) -> and [ ]
    (GDivide,GDivide) -> and [ ]
    (GEq,GEq) -> and [ ]
    (GGe,GGe) -> and [ ]
    (GGt,GGt) -> and [ ]
    (GLe,GLe) -> and [ ]
    (GLt,GLt) -> and [ ]
    (GMinus,GMinus) -> and [ ]
    (GModulo,GModulo) -> and [ ]
    (GMult,GMult) -> and [ ]
    (GNe,GNe) -> and [ ]
    (GOr,GOr) -> and [ ]
    (GPlus,GPlus) -> and [ ]
    (GBinExpr x1 x2 x3,GBinExpr y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GFun x1 x2 x3 x4,GFun y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GFunApp x1 x2,GFunApp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIfThenElse x1 x2 x3,GIfThenElse y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNormIsInfringed x1,GNormIsInfringed y1) -> and [ x1 == y1 ]
    (GPredApp x1 x2,GPredApp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPredicate x1 x2 x3 x4,GPredicate y1 y2 y3 y4) -> and [ x1 == y1 , x2 == y2 , x3 == y3 , x4 == y4 ]
    (GProject x1 x2,GProject y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUnary x1 x2,GUnary y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GVar x1,GVar y1) -> and [ x1 == y1 ]
    (GListExpr x1,GListExpr y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListName x1,GListName y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListRowTypeDecl x1,GListRowTypeDecl y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GMkMetadata x1,GMkMetadata y1) -> and [ x1 == y1 ]
    (GNoMetadata,GNoMetadata) -> and [ ]
    (GMkName x1,GMkName y1) -> and [ x1 == y1 ]
    (GMkRowDecl x1 x2,GMkRowDecl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMkRowTypeDecl x1 x2 x3,GMkRowTypeDecl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GEmptyS,GEmptyS) -> and [ ]
    (GExprS x1,GExprS y1) -> and [ x1 == y1 ]
    (GTypeDeclS x1,GTypeDeclS y1) -> and [ x1 == y1 ]
    (GMkTypeDecl x1 x2 x3,GMkTypeDecl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GNot,GNot) -> and [ ]
    (GUnaryMinus,GUnaryMinus) -> and [ ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GBinOp where
  gf GAnd = mkApp (mkCId "And") []
  gf GDivide = mkApp (mkCId "Divide") []
  gf GEq = mkApp (mkCId "Eq") []
  gf GGe = mkApp (mkCId "Ge") []
  gf GGt = mkApp (mkCId "Gt") []
  gf GLe = mkApp (mkCId "Le") []
  gf GLt = mkApp (mkCId "Lt") []
  gf GMinus = mkApp (mkCId "Minus") []
  gf GModulo = mkApp (mkCId "Modulo") []
  gf GMult = mkApp (mkCId "Mult") []
  gf GNe = mkApp (mkCId "Ne") []
  gf GOr = mkApp (mkCId "Or") []
  gf GPlus = mkApp (mkCId "Plus") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> GAnd 
      Just (i,[]) | i == mkCId "Divide" -> GDivide 
      Just (i,[]) | i == mkCId "Eq" -> GEq 
      Just (i,[]) | i == mkCId "Ge" -> GGe 
      Just (i,[]) | i == mkCId "Gt" -> GGt 
      Just (i,[]) | i == mkCId "Le" -> GLe 
      Just (i,[]) | i == mkCId "Lt" -> GLt 
      Just (i,[]) | i == mkCId "Minus" -> GMinus 
      Just (i,[]) | i == mkCId "Modulo" -> GModulo 
      Just (i,[]) | i == mkCId "Mult" -> GMult 
      Just (i,[]) | i == mkCId "Ne" -> GNe 
      Just (i,[]) | i == mkCId "Or" -> GOr 
      Just (i,[]) | i == mkCId "Plus" -> GPlus 


      _ -> error ("no BinOp " ++ show t)

instance Gf GExpr where
  gf (GBinExpr x1 x2 x3) = mkApp (mkCId "BinExpr") [gf x1, gf x2, gf x3]
  gf (GFun x1 x2 x3 x4) = mkApp (mkCId "Fun") [gf x1, gf x2, gf x3, gf x4]
  gf (GFunApp x1 x2) = mkApp (mkCId "FunApp") [gf x1, gf x2]
  gf (GIfThenElse x1 x2 x3) = mkApp (mkCId "IfThenElse") [gf x1, gf x2, gf x3]
  gf (GNormIsInfringed x1) = mkApp (mkCId "NormIsInfringed") [gf x1]
  gf (GPredApp x1 x2) = mkApp (mkCId "PredApp") [gf x1, gf x2]
  gf (GPredicate x1 x2 x3 x4) = mkApp (mkCId "Predicate") [gf x1, gf x2, gf x3, gf x4]
  gf (GProject x1 x2) = mkApp (mkCId "Project") [gf x1, gf x2]
  gf (GUnary x1 x2) = mkApp (mkCId "Unary") [gf x1, gf x2]
  gf (GVar x1) = mkApp (mkCId "Var") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "BinExpr" -> GBinExpr (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Fun" -> GFun (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "FunApp" -> GFunApp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "IfThenElse" -> GIfThenElse (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "NormIsInfringed" -> GNormIsInfringed (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PredApp" -> GPredApp (fg x1) (fg x2)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "Predicate" -> GPredicate (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2]) | i == mkCId "Project" -> GProject (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Unary" -> GUnary (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Var" -> GVar (fg x1)


      _ -> error ("no Expr " ++ show t)

instance Gf GListExpr where
  gf (GListExpr []) = mkApp (mkCId "BaseExpr") []
  gf (GListExpr (x:xs)) = mkApp (mkCId "ConsExpr") [gf x, gf (GListExpr xs)]
  fg t =
    GListExpr (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseExpr" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsExpr" -> fg x1 : fgs x2


      _ -> error ("no ListExpr " ++ show t)

instance Gf GListName where
  gf (GListName []) = mkApp (mkCId "BaseName") []
  gf (GListName (x:xs)) = mkApp (mkCId "ConsName") [gf x, gf (GListName xs)]
  fg t =
    GListName (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseName" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsName" -> fg x1 : fgs x2


      _ -> error ("no ListName " ++ show t)

instance Gf GListRowTypeDecl where
  gf (GListRowTypeDecl []) = mkApp (mkCId "BaseRowTypeDecl") []
  gf (GListRowTypeDecl (x:xs)) = mkApp (mkCId "ConsRowTypeDecl") [gf x, gf (GListRowTypeDecl xs)]
  fg t =
    GListRowTypeDecl (fgs t) where
     fgs t = case unApp t of
      Just (i,[]) | i == mkCId "BaseRowTypeDecl" -> []
      Just (i,[x1,x2]) | i == mkCId "ConsRowTypeDecl" -> fg x1 : fgs x2


      _ -> error ("no ListRowTypeDecl " ++ show t)

instance Gf GMetadata where
  gf (GMkMetadata x1) = mkApp (mkCId "MkMetadata") [gf x1]
  gf GNoMetadata = mkApp (mkCId "NoMetadata") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkMetadata" -> GMkMetadata (fg x1)
      Just (i,[]) | i == mkCId "NoMetadata" -> GNoMetadata 


      _ -> error ("no Metadata " ++ show t)

instance Gf GName where
  gf (GMkName x1) = mkApp (mkCId "MkName") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkName" -> GMkName (fg x1)


      _ -> error ("no Name " ++ show t)

instance Gf GRowTypeDecl where
  gf (GMkRowDecl x1 x2) = mkApp (mkCId "MkRowDecl") [gf x1, gf x2]
  gf (GMkRowTypeDecl x1 x2 x3) = mkApp (mkCId "MkRowTypeDecl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MkRowDecl" -> GMkRowDecl (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "MkRowTypeDecl" -> GMkRowTypeDecl (fg x1) (fg x2) (fg x3)


      _ -> error ("no RowTypeDecl " ++ show t)

instance Gf GS where
  gf GEmptyS = mkApp (mkCId "EmptyS") []
  gf (GExprS x1) = mkApp (mkCId "ExprS") [gf x1]
  gf (GTypeDeclS x1) = mkApp (mkCId "TypeDeclS") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EmptyS" -> GEmptyS 
      Just (i,[x1]) | i == mkCId "ExprS" -> GExprS (fg x1)
      Just (i,[x1]) | i == mkCId "TypeDeclS" -> GTypeDeclS (fg x1)


      _ -> error ("no S " ++ show t)

instance Gf GTypeDecl where
  gf (GMkTypeDecl x1 x2 x3) = mkApp (mkCId "MkTypeDecl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkTypeDecl" -> GMkTypeDecl (fg x1) (fg x2) (fg x3)


      _ -> error ("no TypeDecl " ++ show t)

instance Gf GUnaryOp where
  gf GNot = mkApp (mkCId "Not") []
  gf GUnaryMinus = mkApp (mkCId "UnaryMinus") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Not" -> GNot 
      Just (i,[]) | i == mkCId "UnaryMinus" -> GUnaryMinus 


      _ -> error ("no UnaryOp " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GBinExpr x1 x2 x3 -> r GBinExpr `a` f x1 `a` f x2 `a` f x3
    GFun x1 x2 x3 x4 -> r GFun `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GFunApp x1 x2 -> r GFunApp `a` f x1 `a` f x2
    GIfThenElse x1 x2 x3 -> r GIfThenElse `a` f x1 `a` f x2 `a` f x3
    GNormIsInfringed x1 -> r GNormIsInfringed `a` f x1
    GPredApp x1 x2 -> r GPredApp `a` f x1 `a` f x2
    GPredicate x1 x2 x3 x4 -> r GPredicate `a` f x1 `a` f x2 `a` f x3 `a` f x4
    GProject x1 x2 -> r GProject `a` f x1 `a` f x2
    GUnary x1 x2 -> r GUnary `a` f x1 `a` f x2
    GVar x1 -> r GVar `a` f x1
    GMkMetadata x1 -> r GMkMetadata `a` f x1
    GMkName x1 -> r GMkName `a` f x1
    GMkRowDecl x1 x2 -> r GMkRowDecl `a` f x1 `a` f x2
    GMkRowTypeDecl x1 x2 x3 -> r GMkRowTypeDecl `a` f x1 `a` f x2 `a` f x3
    GExprS x1 -> r GExprS `a` f x1
    GTypeDeclS x1 -> r GTypeDeclS `a` f x1
    GMkTypeDecl x1 x2 x3 -> r GMkTypeDecl `a` f x1 `a` f x2 `a` f x3
    GListExpr x1 -> r GListExpr `a` foldr (a . a (r (:)) . f) (r []) x1
    GListName x1 -> r GListName `a` foldr (a . a (r (:)) . f) (r []) x1
    GListRowTypeDecl x1 -> r GListRowTypeDecl `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
