{-# LANGUAGE PatternSynonyms #-}

module ContractAutomaton where

import           Data.Map               (Map)
-- import qualified Data.Map  as Map
import           Automata
import           Control.Monad.Identity (Identity (..))
import           Data.Coerce            (coerce)
import qualified Data.List.NonEmpty     as NE
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Name
import           Syntax
--------------------------
  -- Contract Automaton
--------------------------

{- | A contract automaton S is
    a total and deterministic multi-action automaton with
    S = ⟨Σ , Q, q0, ->⟩,
    together with a total function contract ∈ Q -> 2^Clause assigning a set of clauses to each state.

    Some of the fields here, e.g. @contract@, might be better placed 
    in an @Eval@ monad; just doing it this way to remain closer to the paper.
-}
data ContractAutomaton =
  MkContractAutomaton {
      baseAut  :: DFA StateName Trace
    , contract :: Map StateName StateInfo 
      -- ^ can also just think of a state as being the product of a StateName and StateInfo (i.e., the clauses active in that state)
      -- TODO: Think more about whether to just make Contract a possibly empty list and then contract :: Map StateName Contract
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

contractToStateInfo :: Contract -> StateInfo
contractToStateInfo = coerce . Set.fromList . NE.toList . coerce

----------------
  -- StateName
----------------

type StateName = Name
