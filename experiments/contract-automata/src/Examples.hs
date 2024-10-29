{-# LANGUAGE OverloadedRecordDot #-}

module Examples where

import Automata
import ContractAutomaton
import Syntax
import Eval
import Data.List.NonEmpty as NE
import Control.Monad.Identity (Identity(..))

actionA :: Action
actionA = "a"

actionB :: Action
actionB = "b"

party1 :: Party
party1 = MkParty "1"

party2 :: Party
party2 = MkParty "2"


-------------------------
  -- example from p. 31
--------------------------

eventParty1DoesA :: Event
eventParty1DoesA = MkEvent party1 actionA

eventParty2DoesB :: Event
eventParty2DoesB = MkEvent party2 actionB

autExampleFromPage31 :: ContractAutomaton
autExampleFromPage31 = mkContractAut (contractToCAState contractPage31Example)

-- | ψ := [a]O(b) ∧ [b]F(a), from p. 31
contractPage31Example :: Contract
contractPage31Example = MkContract $ NE.fromList [
  If (GDone eventParty1DoesA) (Must eventParty2DoesB),
  If (GDone eventParty2DoesB) (Shant eventParty1DoesA)]

runPage31AutOnTrace :: [Trace] -> Identity CAState
runPage31AutOnTrace = run autExampleFromPage31.aut

tracePage31Example :: Trace
tracePage31Example = [eventParty1DoesA, eventParty2DoesB]

runPage31AutOnGivenTrace :: Identity CAState
runPage31AutOnGivenTrace = runPage31AutOnTrace [tracePage31Example]

residualOnPage31Example :: [Clause]
residualOnPage31Example = residual (contractToClauses contractPage31Example) tracePage31Example 

{-
λ> residualOnPage31Example

[Must (MkEvent {getActor = MkParty {getName = "2"}, getAction = "b"}),
Shant (MkEvent {getActor = MkParty {getName = "1"}, getAction = "a"})]

λ> runPage31AutOnGivenTrace

Identity (MkCAState {
    clauses = [Must (MkEvent {getActor = MkParty {getName = "2"}, getAction = "b"}),
              Shant (MkEvent {getActor = MkParty {getName = "1"}, getAction = "a"})]})
-}