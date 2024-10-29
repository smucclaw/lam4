module Syntax where

import Data.Text as T
import Data.List.NonEmpty

{-----------
  Resources
-------------

* "Contract Automata: An Operational View of Contracts Between Interactive Parties"

* https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell

SCL:
* https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Syntax.hs
* "SCL: A domain-specific language for normative texts with timing constraints"
-}


{- | A Contract := one or more clauses (if more than one, then it represents the *conjunction* of those clauses). 
The sense in which I'm using 'contract' is more like Camilieri's than the CA paper (which sometimes treats 'contract' as being synonymous with 'clause').
-}
newtype Contract = MkContract { clauses :: NonEmpty Clause }
  deriving newtype (Eq, Ord)
  deriving stock Show

--------------
--  Clause
--------------

{- | CL_rest.
        C := O⊥(a) | P(a) | F⊥(a) | C∧C | [β]C | ⊤ | ⊥

    CL_rest does not have ⊕ (exclusive or) between deontic operators.

     Conjunction has been factored out to Contract -- a conjunction of clauses should be represented as a sequence of them (wrapped in a Contract).
-}
data Clause = Must   Party ActionForNorm   -- ^ Obligation
            | May    Party ActionForNorm   -- ^ Permission
            -- TODO: Think about whether to add a construct tt allows user to specify that these are the *only* options available to the actor
            | Shant  Party ActionForNorm   -- ^ Prohibition
            | If     Guard  Clause         -- ^ [β]C -- the contract C must be executed if action β is performed
            | Top                          -- ^ ⊤
            | Bottom                       -- ^ ⊥
  deriving (Eq, Ord, Show)


-----------------------
---  Actions
-----------------------

-- | The notion of actions that's used with norms. In CL_rest, modalities (O, P and F) are only applied to atomic actions (CA p.28)
type ActionForNorm = AtomicAction

{- | An AtomicAction, as I'm using it, does *not* include the agent / party.
Note that I differ from Camilieri's SCL on this: he uses "the term action to mean both agent and act together".
-}
type AtomicAction = Text

type Guard = CompoundAction

{- | β := 0 | 1 | a | !a | β&β |β.β |β∗
  I omit the concurrency operator @&@ because
    (i) truly simultaneous actions are rare in the legal / regulation context
    (ii) it can be simulated with interleaving
    (iii) according to "A framework for conflict analysis of normative texts written
    in controlled natural language", it results in exponential blowup
  
  TODO: Add !a
-}
data CompoundAction =  AtomicAction AtomicAction                  -- ^ a
                    | Sequence     CompoundAction CompoundAction
                    -- | Impossible                                  -- ^ the impossible action, aka 0
                    -- | Skip                                        -- ^ skip, aka 1: matches any action
  deriving (Eq, Ord, Show)


-----------------------
---  Traces, Events
-----------------------

-- | A trace is a sequence of events
newtype Trace = MkTrace { getTrace :: [Event] }
  deriving newtype (Eq, Ord)
  deriving stock Show

{- |
"We assume that
  * each event is in fact a pair containing the event itself and who is the actor (subject, or performer of the event),

  * and that the set Names contains all the possible actors included in a contract (in bilateral contracts as we are having here, there usually be only two subjects, namely the two parties involved in the contract)" (CA p. 30)

"We also assume two projection functions giving the action itself and the subject""
-}
data Event = MkEvent { getActor :: Party
                     , getAction :: ActionForNorm }
  deriving stock (Eq, Ord, Show)


-----------------------
  ---  Party
-----------------------

newtype Party = MkParty { getName :: Text }
  deriving newtype (Eq, Ord)
  deriving stock (Show)
