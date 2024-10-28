{-# LANGUAGE PatternSynonyms #-}

module ContractAutomaton where

import           Data.Map  (Map)
-- import qualified Data.Map  as Map
import           Data.Set  (Set)
-- import qualified Data.Set  as Set

import           Automata
import Control.Monad.Identity (Identity(..))
import           Name
import           Syntax

{- | A contract automaton S is
    a total and deterministic multi-action automaton with
    S = ⟨Σ , Q, q0, →⟩,
    together with a total function contract ∈ Q → 2^Clause assigning a set of clauses to each state.
-}
data ContractAutomaton =
  MkContractAutomaton {
      baseAut  :: DFA StateName Trace
    , contract :: Map StateName StateInfo
  }

-- | Convenience pattern synonym
pattern ContractAutomaton :: 
        (StateName -> Trace -> Identity StateName) -- transition function
        -> StateName                               -- initial state
        -> (StateName -> Bool)                     -- accepting states predicate
        -> Map StateName StateInfo                 -- contract
        -> ContractAutomaton
pattern ContractAutomaton trans initial acc contract =
    MkContractAutomaton (Automaton initial trans acc) contract

------------------------------------
  -- Contract Automaton State Info
------------------------------------

-- | The sorts of info associated with a state of a contract automaton
newtype StateInfo = MkStateInfo { activeClauses :: Set Clause }
  deriving newtype (Eq, Ord)
  deriving stock Show

----------------
  -- StateName
----------------

type StateName = Name
