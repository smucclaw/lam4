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
  , BuiltinTypeForRelation(..)
  , Statement(..)
  , Deontic(..)
  , ToplevelElement(..))
  where

import           Base
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.Name         (Name (..))

data ToplevelElement = DeclElt Decl | StatementElt Statement
  deriving stock (Show, Eq, Ord)

data Decl =
    NonRec Name Expr
  | Rec    Name Expr
  deriving stock (Show, Eq, Ord)

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


data Statement
  = IfStmt Expr (NonEmpty Statement) [Statement] -- If Condition Then Otherwise
  | Assign Name Expr

  {- WIP deontics-related things
    Thinking of deontic stuff as statements agrees,
    not just with work in AI and AI x Law,
    but also with a longstanding tradition in philosophy and linguistics 
    that models them as being fundamentally *non*-truth-conditional (e.g., as 'proposals to update the conversational scoreboard')
  -}
  | Action Name [Name] [Statement] -- Action NameOfAction Params Body(block of statements)
  | Norm   Deontic
  deriving stock (Show, Eq, Ord)


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

  Note:
  * Using 'norm' to mean something potentially broader than a 'deontic'
-}

{-| Think of this, in the simplest case, as:
      @
      <agent>
      MUST/MAY
      <action>
      @

    Or with a condition / trigger (using IfStmt):
      @
      IF <trigger event>
      THEN <agent>
            MUST/MAY
            <action>
      @

    And potentially with a deadline (though that's probably for v2 / v3):
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
  MkDeontic { name         :: Name -- ^ Identifier for the deontic; may not need this given that Actions in effect are a way to name statements
            , agent        :: Name -- in the future may want to be able to quantify over agents too
            , deonticModal :: DeonticModal
            , action       :: Statement
            -- , deadline     :: WIP_NotSureYet -- only in v2 / v3
            }
  deriving stock (Eq, Show, Ord)

data WIP_NotSureYet
  deriving stock (Eq, Show, Ord)

data DeonticModal = Obligation | Permission
  deriving stock (Eq, Show, Ord)
