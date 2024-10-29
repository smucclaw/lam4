module ContractAutomaton (ContractAutomaton(..), acceptingPred, CAState(..), contractToCAState) where

import           Automata
import           Syntax
-- import           Name

import           Data.Coerce            (coerce)
import qualified Data.List.NonEmpty     as NE
-- import           Control.Monad.Identity (Identity (..))
-- import           Data.Set               (Set)
-- import qualified Data.Set               as Set
-- import           Data.Map               (Map)
-- import qualified Data.Map  as Map

--------------------------
  -- Contract Automaton
--------------------------

-- TODO: This prob has to be a NFA if we want to remove the & concurrency operator and use interleaving instead?
{- | "A contract automaton S is
    a total and deterministic multi-action automaton with
    S = ⟨Σ , Q, q0, ->⟩,
    together with a total function contract ∈ Q -> 2^Clause assigning a set of clauses to each state.

    "The set of states Q is given by all the formulae R reachable from [the initial formula / state] ψ"
-}
newtype ContractAutomaton = MkContractAut { aut :: DFA CAState Event }

acceptingPred :: CAState -> Bool
acceptingPred (MkCAState clauses) = all (== Top) clauses

{- TODO
For a contract with action alphabet Σ, we will introduce its deontic alphabet Σd which consists of Oa, Pa and Fa for each action a ∈ Σ, that will be used to represent which normative behaviour is enacted at a particular moment.
-}

------------------------------------
  -- Contract Automaton State
------------------------------------

-- | The state of a contract automaton. It is possible for a CA state to contain 0 clauses. 
newtype CAState = MkCAState { clauses :: [Clause] }
  deriving newtype (Eq, Ord)
  deriving stock Show

contractToCAState :: Contract -> CAState
contractToCAState (MkContract clauses) = coerce $ NE.toList clauses

----------------
  -- StateName
----------------

-- TODO: Defer support for explicitly naming states to a future draft
-- type StateName = Name
