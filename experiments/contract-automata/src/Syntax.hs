module Syntax where

{-----------
  Resources
-------------

* "Contract Automata: An Operational View of Contracts Between Interactive Parties"

* https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell

SCL:
* https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Syntax.hs
* "SCL: A domain-specific language for normative texts with timing constraints"
-}

--------------
--  Clause 
--------------

{- | CL_rest.
      C := O⊥(a) | P(a) | F⊥(a) | C∧C | [β]C | ⊤ | ⊥
-}
data Clause = Must   Action        -- ^ Obligation
            | May    Action        -- ^ Permission
            | Cannot Action        -- ^ Prohibition
            | And    Clause Clause -- ^ Conjunction of clauses
            | If     Guard  Clause -- ^ [β]C -- the contract C must be executed if action β is performed
            | Top                  -- ^ ⊤
            | Bottom               -- ^ ⊥
  deriving (Eq, Ord, Show)


-----------------------
---  Guards, Actions
-----------------------

newtype Guard = MkGuard { getGuard :: Action }
  deriving newtype (Eq, Ord)
  deriving stock Show

type AtomicAction = String

{- | β := 0|1|a|a| β&β |β.β |β∗
  I omit the concurrency operator @&@ because
    (i) truly simultaneous actions are rare in the legal / regulation context
    (ii) it can be simulated with interleaving
    (iii) according to "A framework for conflict analysis of normative texts written
    in controlled natural language", it results in exponential blowup
-}
data Action = Impossible                 -- ^ 0
            | Skip                       -- ^ 1
            | AtomicAction AtomicAction  -- ^ a
            | Sequence     Action Action
  deriving (Eq, Ord, Show)
