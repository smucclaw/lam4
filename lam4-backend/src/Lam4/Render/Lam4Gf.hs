{-# LANGUAGE GADTs, UndecidableInstances #-}
module Lam4.Render.Lam4Gf where

import Control.Monad.Identity
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

type GListRowTypeDecl = Tree GListRowTypeDecl_
data GListRowTypeDecl_
type GMetadata = Tree GMetadata_
data GMetadata_
type GRowTypeDecl = Tree GRowTypeDecl_
data GRowTypeDecl_
type GTypeDecl = Tree GTypeDecl_
data GTypeDecl_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GListRowTypeDecl :: [GRowTypeDecl] -> Tree GListRowTypeDecl_
  GMkMetadata :: GString -> Tree GMetadata_
  GNoMetadata :: Tree GMetadata_
  GMkRowDecl :: GMetadata -> GString -> Tree GRowTypeDecl_
  GMkRowTypeDecl :: GMetadata -> GString -> GString -> Tree GRowTypeDecl_
  GMkTypeDecl :: GMetadata -> GString -> GListRowTypeDecl -> Tree GTypeDecl_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GListRowTypeDecl x1,GListRowTypeDecl y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GMkMetadata x1,GMkMetadata y1) -> and [ x1 == y1 ]
    (GNoMetadata,GNoMetadata) -> and [ ]
    (GMkRowDecl x1 x2,GMkRowDecl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMkRowTypeDecl x1 x2 x3,GMkRowTypeDecl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GMkTypeDecl x1 x2 x3,GMkTypeDecl y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

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

instance Gf GRowTypeDecl where
  gf (GMkRowDecl x1 x2) = mkApp (mkCId "MkRowDecl") [gf x1, gf x2]
  gf (GMkRowTypeDecl x1 x2 x3) = mkApp (mkCId "MkRowTypeDecl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MkRowDecl" -> GMkRowDecl (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "MkRowTypeDecl" -> GMkRowTypeDecl (fg x1) (fg x2) (fg x3)


      _ -> error ("no RowTypeDecl " ++ show t)

instance Gf GTypeDecl where
  gf (GMkTypeDecl x1 x2 x3) = mkApp (mkCId "MkTypeDecl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkTypeDecl" -> GMkTypeDecl (fg x1) (fg x2) (fg x3)


      _ -> error ("no TypeDecl " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GMkMetadata x1 -> r GMkMetadata `a` f x1
    GMkRowDecl x1 x2 -> r GMkRowDecl `a` f x1 `a` f x2
    GMkRowTypeDecl x1 x2 x3 -> r GMkRowTypeDecl `a` f x1 `a` f x2 `a` f x3
    GMkTypeDecl x1 x2 x3 -> r GMkTypeDecl `a` f x1 `a` f x2 `a` f x3
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
