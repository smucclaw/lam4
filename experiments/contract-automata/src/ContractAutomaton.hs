module ContractAutomaton where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import           Automata
import           Syntax


{- | A contract automaton S is
    a total and deterministic multi-action automaton with
    S = ⟨Σ , Q, q0, →⟩,
    together with a total function contract ∈ Q → 2^Clause assigning a set of clauses to each state.
-}
data ContractAutomaton =
  MkContractAutomaton {
      baseAut  :: DFA StateName Trace
    , contract :: Map StateName (Set Clause)
  }

-------------------------------
  -- Contract Automaton State
-------------------------------

-- | CAStateInfo stands for 'Contract Automaton State Info' --- i.e., the sorts of info associated with a state of a contract automaton
newtype CAStateInfo = MkCAState { activeClauses :: Set Clause }
  deriving newtype (Eq, Ord)
  deriving stock Show

-------------
  -- Name
-------------

type Unique = Int
data Name = MkName { name :: T.Text, unique :: Unique }
type StateName = Name

instance Eq Name where
  (==) (MkName _ u) (MkName _ u') = u == u'

instance Ord Name where
  compare (MkName _ u) (MkName _ u') = compare u u'

instance Show Name where
  show (MkName n u) = T.unpack n <> "_" <> show u
