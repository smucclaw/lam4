{-
TODO:
* Add Builtin list operations
-}
module Lam4.Expr.ConcreteSyntax where

import           Base
import           Lam4.Expr.Name (Name (..))


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
  | Literal    Literal
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
  | NormExpr   [NormativeClause]

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

type Row a = [(Name, a)]

{- | References to where in the original source this corresponds to; e.g., §10.
     WIP -- In the future, the structure will be more complicated.
     Want to be able to support things like `§10(1)(a)`
-}
newtype OriginalRuleRef
  = MkOriginalRuleRef Text
  deriving newtype (Eq, Ord)
  deriving stock (Show)


data Relatum
  = CustomType  Name
  | BuiltinType BuiltinTypeForRelation
  deriving stock (Eq, Show, Ord, Generic)

data BuiltinTypeForRelation = BuiltinTypeString | BuiltinTypeInteger | BuiltinTypeBoolean
  deriving stock (Eq, Show, Ord, Generic)

-- TODO: tweak the grammar to distinguish between integers and non-integers
data Literal
  = IntegerLiteral Integer
  | BooleanLiteral Bool
  | StringLiteral Text
  deriving stock (Eq, Show, Ord)

{-
TO ADD:
 Modulo / (integer) remainder
-}
data BinOp
  = Or
  | And
  | Plus
  | Minus
  | Mult
  | Div
  | Lt
  | Lte
  | Gt
  | Gte
  | Equals
  | NotEquals
   deriving stock (Eq, Show, Ord)

data UnaryOp = Not | UnaryMinus
  deriving stock (Eq, Show, Ord)


{-===========================================================
   Normative clauses
=============================================================-}

{- | WIP. Trying to synthesize
  * "Modelling and Analysis of Normative Documents"
  * "COIR: Verifying Normative Specifications of Complex Systems"
  * The work on SLEEC
-}
data NormativeClause = NCDeontic Deontic
                     | NCRef Name -- ^ Reference to another clause
    deriving stock (Eq, Show, Ord)

{-| Think of this, in the simplest case, as:

      @
      WHEN <trigger event>
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
