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

runPage31AutOnTrace :: Trace -> Identity CAState
runPage31AutOnTrace = run autExampleFromPage31.aut

tracePage31Example :: Trace
tracePage31Example = [eventParty1DoesA, eventParty2DoesB]

-- | This is not exactly the given example, since the given example is a trace containing a set of concurrent actions
runPage31AutOnGivenTrace :: Identity CAState
runPage31AutOnGivenTrace = runPage31AutOnTrace tracePage31Example

-- Stepping through the example, event by event

residualOnPage31ExampleOnce :: [Clause]
residualOnPage31ExampleOnce = residual (contractToClauses contractPage31Example) eventParty1DoesA

residualOnPage31ExampleTwice :: [Clause]
residualOnPage31ExampleTwice = residual residualOnPage31ExampleOnce eventParty2DoesB

{-
λ> residualOnPage31ExampleOnce
[Must (MkEvent {getActor = MkParty {getName = "2"}, getAction = "b"}),Top]
λ> residualOnPage31ExampleTwice
[Top,Top]

λ> runPage31AutOnGivenTrace
Identity (MkCAState {clauses = [Top,Top]})
-}