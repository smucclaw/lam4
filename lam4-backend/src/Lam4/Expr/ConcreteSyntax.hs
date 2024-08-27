{-
TODO:
* Add Builtin list operations
-}
module Lam4.Expr.ConcreteSyntax
  (
  -- * Decl and convenience constructors
    Decl
  , mkStatementBlockDecl
  , mkSingletonStatementDecl

  -- * Expr
  , Expr(..)
  , Lit(..)
  , OriginalRuleRef(..)
  , UnaryOp(..)
  , BinOp(..)
  , Relatum(..)
  , BuiltinTypeForRelation(..)

  -- * Statements
  , Statement(..)
  , DeonticModal(..)
)
  where

import           Base                   hiding (singleton)
import           Base.NonEmpty          (singleton)
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.Name         (Name (..))
-- a Name can refer to an Expr or a Statement (but note that not all kinds of Statements can have names)

type Decl = DeclF Expr

mkStatementBlockDecl :: Name -> NonEmpty Statement -> Decl
mkStatementBlockDecl name statements = NonRec name $ StatementBlock statements

mkSingletonStatementDecl :: Name -> Statement -> Decl
mkSingletonStatementDecl name statement = mkStatementBlockDecl name $ singleton statement

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
  | Let        Decl Expr
  | StatementBlock  (NonEmpty Statement)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}
  | NormIsInfringed Name                           -- NormIsInfringed NameOfNorm.
                                                   -- Sugar for a predicate checking if @nameOfNorm@ is violated (users can supply unique identifiers for Deontics and IfThenOtherwise statements that contain a Deontic)

  | Predicate  [Name] Expr (Maybe OriginalRuleRef) -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | PredApp    Expr [Expr]

  {--------------------------
    Sigs and Relations
  ---------------------------
  An Alloy Sig is a set
  Had added  sigs and relations
  because they're useful for certain sorts of analyses;
  but not sure that we really want them
  -}
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


  {- | ------------------------------------------------
  General background / context for the normative stuff
  -----------------------------------------------------
  One way of adding normative stuff to a functional expression language amounts to augmenting the evaluator
  with a Store / EvalState; and in particular, keeping track of the statuses of the normative stuff
  (e.g. what obligations are operative at any point; what obligations have been satisfied or violated; what actions have been taken),
  as well as whatever other state is required.

  It is for this reason that I'm modelling normative stuff as *statements* instead of *expressions*.
  Thinking of them as statements also agrees,
  not just with work in AI and AI x Law,
  but also with a longstanding tradition in philosophy and linguistics
  that models them as being fundamentally *non*-truth-conditional, stateful things
  (e.g., as 'proposals to update the conversational scoreboard').

  -----------
    Actions
  -----------
  Actions on this framework are basically impure functions. When an action is taken, the Store/EvalState is mutated in the necessary ways.

  Can think of actions also as 'events'.
  This is perhaps clearest in the case of an 'atomic' action where there are no statements in its body.
  Such atomic actions would be very similar to events in the SLEEC language.

  If you prefer to think in terms of transitions, this quote from John Camilleri may be helpful:
    > An action is simply a transition which can only be taken when the corresponding action ... has been performed [by the agent in question].
    > Each action also gets a corresponding *doer* automaton which sets the status of that action to *done*

  Misc:
  * Currently distinguishing between Actions and Functions in the surface syntax,
    because I think that having different syntax for pure vs impure functions will be helpful for end users. But not sure.

  ----------
    Norms
  ----------
  Trying to synthesize
  * "Modelling and Analysis of Normative Documents"
  * "COIR: Verifying Normative Specifications of Complex Systems"
  * The work on SLEEC

  Think of this, in the simplest case, as:
      @
      <agent>
      MUST/MAY
      <action>
      @

    Or with a condition / trigger (using IfStatement):
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

    (For simpler use cases, deadlines/temporal constraints can also be simulated with atomic / more coarse-grained actions.)

  Note:
  * Using 'norm' to mean something potentially broader than a 'deontic'
-}
data Statement
  = IfStatement Expr (NonEmpty Statement) [Statement] -- If Condition Then Otherwise
  | Assign Name Expr
  | Action Name [Name] [Statement]                    -- Action NameOfAction Params Body(block of statements)
  | Norm   Name DeonticModal Statement                -- Norm   Agent DeonticModal Action. Can add deadlines in v2 / v3
                                                      -- Note: Users can supply a name for the Norm
                                                      -- if it doesn't appear in the scope of another Statement that already has a Name

  | Breach                                            -- Like CSL's @failure@. Not sure we need this though; WIP.

  deriving stock (Show, Eq, Ord)


{-===========================================================
   Deontic
=============================================================-}

-- | Can add prohibitions as sugar in the future
data DeonticModal = Obligation | Permission
  deriving stock (Eq, Show, Ord)
