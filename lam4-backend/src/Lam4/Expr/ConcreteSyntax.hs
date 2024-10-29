{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module Lam4.Expr.ConcreteSyntax
  (
  -- re-exports from common syntax
    TypeExpr(..)
  , TyBuiltin(..)
  , RowTypeDecl(..)
  -- * Decl and convenience constructors
  , Decl
  , DeclF(..)
  , mkStatementBlockDecl
  , mkSingletonStatementDecl
  , mkRecordDecl

  -- * Expr
  , Expr(..)
  , Lit(..)
  , OriginalRuleRef(..)
  , UnaryOp(..)
  , BinOp(..)
  , CSTProgram

  -- * Statements
  , Statement(..)
  , DeonticModal(..)

  -- * Traversals
  , exprSubexprs
  , exprSubexprsVL
)
  where

import           Base                   hiding (singleton)
import           Base.NonEmpty          (singleton)
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.Name         (Name (..))

-- a Name can refer to an Expr or a Statement (but note that not all kinds of Statements can have names)

type CSTProgram = [Decl]
type Decl = DeclF Expr

-- TODO: Think about whether to use pattern synonyms instead, re the following constructor functions

mkStatementBlockDecl :: Name -> NonEmpty Statement -> Decl
mkStatementBlockDecl name statements = NonRec name $ StatementBlock statements

mkSingletonStatementDecl :: Name -> Statement -> Decl
mkSingletonStatementDecl name statement = mkStatementBlockDecl name $ singleton statement

mkRecordDecl :: Name -> [RowTypeDecl] -> [Name] -> RecordDeclMetadata -> Decl
mkRecordDecl recordName rowTypeDecls parents recordDeclMetadata = DataDecl recordName (RecordDecl rowTypeDecls parents recordDeclMetadata)


{-
TODO:
  High priority
    * Update Langium grammar and typechecker to match new constructs
  Lower priority
    * Add support for Transparency-related knobs
-}
data Expr
  = Var        Name
  | Lit        Lit
  | Cons       Expr Expr                           -- list cons
  | List       [Expr]                              -- construct a list
  | Unary      UnaryOp Expr
  | BinExpr    BinOp Expr Expr
  | IfThenElse Expr Expr Expr
  | FunApp     Expr [Expr]
  | Record     (Row Expr)                          -- record construction
  | Project    Expr Name                           -- record projection
  | Fun        RuleMetadata [Name] Expr            -- Function
  | Let        Decl Expr
  | Foldr      Expr Expr Expr                      -- Foldr combine nil     collection
  | Foldl      Expr Expr Expr                      -- Foldl update  initial collection
  | StatementBlock  (NonEmpty Statement)

  {-===========================
    What follows is
    EXPERIMENTAL or VERY WIP
  ============================-}
  | NormIsInfringed Name                           -- NormIsInfringed NameOfNorm.
                                                   -- This is a predicate that checks if @nameOfNorm@ is violated (users can supply unique identifiers for Deontics and IfThenOtherwise statements that contain a Deontic)

-- TODO: Add type sigs into ConcreteSyntax
  | Predicate  RuleMetadata [Name] Expr            -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
  | PredApp    Expr [Expr]

  {--------------------------
    Sigs and Relations
  ---------------------------
  An Alloy Sig is a set
  Had added  sigs and relations
  because they're useful for certain sorts of analyses;
  but not sure that we really want them
  -}
  -- The currently supported Sigs are 'ONE' Sigs (and only those)
  | Sig        [Name] [Expr]                       -- Sig parents relations
  -- Sep 2024: Join is currently disabled; de-emphasizing these things for now
  -- | Join       Expr Expr                           -- Relational join
  | Relation   Name Name TypeExpr (Maybe Text)      -- Relation relName relParentSigName relatum description
  deriving stock (Eq, Show, Ord)

-- TODO: tweak the grammar to distinguish between integers and non-integers
data Lit
  = IntLit    Int
  | FracLit   Double
  | BoolLit   Bool
  | StringLit Text
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
  * Normative Programming Language (NPL) (https://github.com/moise-lang/)

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
  * Don't need a `Breach` / `GotoBreach` because "if the contract states A "must" Q by time T and A does not Q by T, then there is always a breach. A breach triggers second order obligations to make reparations etc, so not doing those does not breach the contract itself, but those second order things" (law expert Jerrold Soh).
  * The frontend will prevent users from binding a name to a Norm if the Norm appears in the scope of another Statement that already has a Name [TODO]
-}
data Statement = IfStatement Expr Statement    [Statement]  -- If   Condition Then         Otherwise
               | Norm        Name DeonticModal Action       -- Norm Agent     DeonticModal Action
    deriving stock (Show, Eq, Ord)                          -- will think about how to add deadline(s) only in v2 / v3


{- | Actions can be given a name with the `Decl` construct

[TODO-Oct29] Not sure now that action blocks / sequences should be blocks of prim actions.
Might want something like Sequence Norm Norm or Sequence Event Event

Think of an ActionBlock as a function with side effects / a function where the statements in the body are PrimActions
(and where a block of actions corresponds to 'sequencing' them in the usual 'imperative language' way)

ActionBlocks need not contain primitive actions; e.g., this Lam4 surface syntax:

@
ACTION `pay for bike`
@

will become a non-recursive Decl with a StatementBlock consisting of an ActionBlock with an empty list of PrimActions.

(Such 'opaque' actions / events are often enough to model a lot of things;
see the SLEEC work, e.g. "Normative Requirements Operationalization with Large Language Models", for some nice examples of this.
This is also a common pattern / idiom in formal modelling in general.)

But for certain modelling purposes, we may want richer structure; e.g.

@
DO
  x increases_by 1
  y increases_by 1
END
@

where @x@ and @y@ are global variables.

Or:

@
ACTION TransferMoolah = DO
  Buyer's  money decreases_by 50
  Seller's money increases_by 50
END
@

------------------

TODO:
  * Think about adding an `unknown` variant, a la https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell/blob/master/UnknownDL.hs
-}
data Action = ActionBlock     (Maybe Name) [Name] [PrimAction] -- ActionBlock NameOfThisActionBlock Params Body(of PrimAction)
                                                               -- Inline action blocks won't have a user-supplied name
            | PrimitiveAction PrimAction
  deriving stock (Show, Eq, Ord)

-- | TODO: Not sure tt we really want ActionRefs in the concrete syntax
data PrimAction = Assign      Name   Expr
                | ActionRef   Name                 -- ActionRef Name (that resolves to an Decl of an Action)
  deriving stock (Show, Eq, Ord)

-- | Can add prohibitions as sugar in the future
data DeonticModal = Obligation | Permission
  deriving stock (Eq, Show, Ord)

makePrisms ''Expr

{- | https://www.michaelpj.com/blog/2020/08/02/lenses-for-tree-traversals.html
"you give it a function that does stuff to subterms,
and it will give you one that does that to all the subterms of a particular term" -}
exprSubexprsVL :: TraversalVL' Expr Expr
exprSubexprsVL f = \case
  -- Exprs with sub-exprs
  Cons first rest                    -> Cons <$> f first <*> f rest
  List xs                            -> List <$> traverse f xs
  Foldr combine nil xs               -> Foldr <$> f combine <*> f nil <*> f xs
  Foldl update initial xs            -> Foldl <$> f update <*> f initial <*> f xs
  Unary op expr                      -> Unary op <$> f expr
  BinExpr op left right              -> BinExpr op <$> f left <*> f right
  IfThenElse cond thn els            -> IfThenElse <$> f cond <*> f thn <*> f els
  FunApp fun args                    -> FunApp <$> f fun <*> traverse f args
  Record rows                        -> Record <$> traverse (\(name, expr) -> (name,) <$> f expr) rows
  Project record label               -> Project <$> f record <*> pure label
  Fun ruleMetadata args body         -> Fun ruleMetadata args <$> f body
  Let decl body                      -> Let decl <$> f body
  Predicate ruleMetadata params body -> Predicate ruleMetadata params <$> f body
  PredApp predicate args             -> PredApp <$> f predicate <*> traverse f args
  Sig parents relations              -> Sig parents <$> traverse f relations
  -- TODO: May need to add a traversal for type exprs
  Relation relName relParentSigName relatum description -> Relation <$> pure relName <*> pure relParentSigName <*> pure relatum <*> pure description
  StatementBlock _                   -> undefined -- TODO

  Var name                           -> pure $ Var name
  Lit lit                            -> pure $ Lit lit
  NormIsInfringed name               -> pure $ NormIsInfringed name

exprSubexprs :: Traversal' Expr Expr
exprSubexprs = traversalVL exprSubexprsVL
