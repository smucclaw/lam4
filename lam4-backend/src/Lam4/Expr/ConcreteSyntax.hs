{-
TODO:
* Add Builtin list operations
-}
module Lam4.Expr.ConcreteSyntax
  ( Decl(..)
  , Expr(..)
  , Lit(..)
  , OriginalRuleRef(..)
  , UnaryOp(..)
  , BinOp(..)
  , Relatum(..)
  , BuiltinTypeForRelation(..)) 
  where

import           Base
import           Lam4.Expr.Name (Name (..))
import          Lam4.Expr.CommonSyntax

data Decl =
    NonRec Name Expr
  | Rec    Name Expr
  deriving stock Show

{-
TODO:
  High priority
    * Work out grammar for normative clauses
    * Update Langium grammar and typechecker to match new constructs
  Lower priority
    * Add support for Transparency-related knobs
-}
data Expr
  = Var        Name
  | Lit        Lit
  | List       [Expr]                              -- construct a list
  | Unary      UnaryOp Expr
  | BinExpr    BinOp Expr Expr
  | IfThenElse Expr Expr Expr
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- | ListExpr   ListOp [Expr]
  | FunApp     Expr [Expr]
  | Record     (Row Expr)                          -- record construction
  | Project    Expr Name                           -- record projection
  | Fun        [Name] Expr (Maybe OriginalRuleRef) -- Function
  | Let        Name Expr Expr
  | Letrec     Name Expr Expr

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}

  -- WIP deontics-related things
  -- | NormExpr   [Norm]

  {------------------------------------------------------------
  Stuff that's more relevant for analysis / symbolic evaluation
  --------------------------------------------------------------
  An Alloy Sig is a set
  Had added  sigs and relations
  because they're useful for certain sorts of analyses;
  but not sure that we really want them
  -}
  | Predicate  [Name] Expr (Maybe OriginalRuleRef) -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | PredApp    Expr [Expr]
  | Sig        [Name] [Expr]                       -- Sig parents relations
  | Join       Expr Expr                           -- Relational join
  | Relation   Name Name Relatum (Maybe Text)      -- Relation relName relParentSigName relatum description
  deriving stock (Eq, Show, Ord)


-- TODO: tweak the grammar to distinguish between integers and non-integers
data Lit
  = IntLit Int
  | BoolLit Bool
  -- | StringLit Text -- TODO: not clear that we need this
  deriving stock (Eq, Show, Ord)


{-===========================================================
   Normative clauses
=============================================================-}

{- | WIP. Trying to synthesize
  * "Modelling and Analysis of Normative Documents"
  * "COIR: Verifying Normative Specifications of Complex Systems"
  * The work on SLEEC
-}
data Norm = NormDeontic Deontic
          | NormRef Name -- ^ Reference to another clause
    deriving stock (Eq, Show, Ord)

{-| Think of this, in the simplest case, as:

      @
      IF <trigger event>
      THEN <agent>
            MUST/MAY
            <action>
      @
  
  And potentially with a deadline:

      @
      IF <trigger event>
      THEN <agent>
            MUST/MAY
            <action>
      BY/WITHIN
      <deadline>
      @
-}
data Deontic =
  MkDeontic { deonticStatus :: DeonticStatus
            , agent         :: WIP_NotSureYet
            , trigger       :: Expr
            , action        :: WIP_NotSureYet
            , deadline      :: [WIP_NotSureYet]}
  deriving stock (Eq, Show, Ord)

data WIP_NotSureYet
  deriving stock (Eq, Show, Ord)

data DeonticStatus = Obligation | Permission
  deriving stock (Eq, Show, Ord)
