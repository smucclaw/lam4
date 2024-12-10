{-# LANGUAGE OverloadedRecordDot #-}

module Examples where

import           Automata
import           ContractAutomaton
import           Control.Monad.Identity (Identity (..))
import           Data.List.NonEmpty     as NE
import           Eval
import           Syntax

--------------------------
  -- example from p. 31
--------------------------
-- Note: The way I handle If Guard Clause might be different from the paper

actionA :: Action
actionA = "a"

actionB :: Action
actionB = "b"

party1 :: Party
party1 = MkParty "1"

party2 :: Party
party2 = MkParty "2"

eventParty1DoesA :: Event
eventParty1DoesA = MkEvent party1 actionA

eventParty2DoesB :: Event
eventParty2DoesB = MkEvent party2 actionB

autExampleFromPage31 :: ContractAutomaton
autExampleFromPage31 = mkContractAut (contractToCAState contractPage31Example) residualWithSimplify

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
residualOnPage31ExampleOnce = residualWithoutSimplify (getClauses contractPage31Example) eventParty1DoesA

residualOnPage31ExampleTwice :: [Clause]
residualOnPage31ExampleTwice = residualWithoutSimplify residualOnPage31ExampleOnce eventParty2DoesB

{-
λ> residualOnPage31ExampleOnce
[Must (MkEvent {getActor = MkParty {getName = "2"}, getAction = "b"}),
 If (GDone (MkEvent {getActor = MkParty {getName = "2"}, getAction = "b"}))
    (Shant (MkEvent {getActor = MkParty {getName = "1"}, getAction = "a"}))]

λ> residualOnPage31ExampleTwice
[Top,
 Shant (MkEvent {getActor = MkParty {getName = "1"}, getAction = "a"})]

λ> recognize autExampleFromPage31.aut tracePage31Example
True

-}

-------------------------
  -- bank example (p.15)
-------------------------

{-
consider John signing a contract with his bank.
The contract says that
(i) whenever he is logged into his Internet banking account,
he is to be permitted to make money transfers; and
(ii) if a malicious attempt to log in to his account is identified,
logging in and making transfers will be prohibited until the situation is cleared
-}

actLogin :: Action
actLogin = "login"

actAttemptHack :: Action
actAttemptHack = "attemptHack"

actResolveSituation :: Action
actResolveSituation = "resolveSituation"

actMakeTransfers :: Action
actMakeTransfers = "makeMoneyTransfers"

partyJohn :: Party
partyJohn = MkParty "John"

partyHacker :: Party
partyHacker = MkParty "Hacker"

partyBank :: Party
partyBank = MkParty "Bank"

eventJohnLogin :: Event
eventJohnLogin = MkEvent partyJohn actLogin

eventHackAttempt :: Event
eventHackAttempt = MkEvent partyHacker actAttemptHack


johnCanLoginAgainIfSituationResolved :: Clause
johnCanLoginAgainIfSituationResolved = If (GDone (MkEvent partyBank actResolveSituation))
                                        (May (MkEvent partyJohn actLogin))

johnCanTransferAgainIfSituationResolved :: Clause
johnCanTransferAgainIfSituationResolved = If (GDone (MkEvent partyBank actResolveSituation))
                                        (May (MkEvent partyJohn actMakeTransfers))

-- | This is not 100% faithful to the original
contractBank :: Contract
contractBank = MkContract $ NE.fromList [
  If (GDone eventJohnLogin) (May (MkEvent partyJohn actMakeTransfers)),
  -- once a hacker attempt is detected, john cannot login or make transfers...
  If (GDone eventHackAttempt) (Shant (MkEvent partyJohn actLogin)),
  If (GDone eventHackAttempt) (Shant (MkEvent partyJohn actMakeTransfers)),
  -- ... unless the situation is resolved
  If (GDone eventHackAttempt) johnCanLoginAgainIfSituationResolved,
  If (GDone eventHackAttempt) johnCanTransferAgainIfSituationResolved
  ]

bankExampleTrace :: [Event]
bankExampleTrace = [eventJohnLogin, eventHackAttempt]

autBankExample :: ContractAutomaton
autBankExample = mkContractAut (contractToCAState contractBank) residualWithoutSimplify

autBankExample' :: Aut Int
autBankExample' = renameAutomaton (buildAutomaton (contractToCAState contractBank))

-- >>> autBankExample'
-- State 0:
--   MkEvent {getActor = MkParty {getName = "Hacker"}, getAction = "attemptHack"} -> 2
--   MkEvent {getActor = MkParty {getName = "John"}, getAction = "login"} -> 5
--   default -> 1
-- <BLANKLINE>
-- State 1:
--   default -> 1
-- <BLANKLINE>
-- State 2:
--   MkEvent {getActor = MkParty {getName = "Bank"}, getAction = "resolveSituation"} -> 3
--   MkEvent {getActor = MkParty {getName = "John"}, getAction = "login"} -> 4
--   MkEvent {getActor = MkParty {getName = "John"}, getAction = "makeMoneyTransfers"} -> 4
--   default -> 1
-- <BLANKLINE>
-- State 3:
--   default -> 1
-- <BLANKLINE>
-- State 4:
--   default -> 4
-- <BLANKLINE>
-- State 5:
--   default -> 1
-- <BLANKLINE>

runBankExampleOnTrace :: Trace -> Identity CAState
runBankExampleOnTrace = run autBankExample.aut

runBankExampleOnGivenTrace :: Identity CAState
runBankExampleOnGivenTrace = runBankExampleOnTrace bankExampleTrace

residualBankExample :: Event -> [Clause]
residualBankExample = residualWithoutSimplify (getClauses contractBank)

{-
The example is setup so that we get the 'conflicting clauses':

λ> runBankExampleOnGivenTrace
Identity
    ( MkCAState
        { clauses =
            [ May
                ( MkEvent
                    { getActor = MkParty
                        { getName = "John" }
                    , getAction = "makeMoneyTransfers"
                    }
                )
            , Shant
                ( MkEvent
                    { getActor = MkParty
                        { getName = "John" }
                    , getAction = "login"
                    }
                )
            , Shant
                ( MkEvent
                    { getActor = MkParty
                        { getName = "John" }
                    , getAction = "makeMoneyTransfers"
                    }
                )
            , If
                ( GDone
                    ( MkEvent
                        { getActor = MkParty
                            { getName = "Bank" }
                        , getAction = "resolveSituation"
                        }
                    )
                )
                ( May
                    ( MkEvent
                        { getActor = MkParty
                            { getName = "John" }
                        , getAction = "login"
                        }
                    )
                )
            , If
                ( GDone
                    ( MkEvent
                        { getActor = MkParty
                            { getName = "Bank" }
                        , getAction = "resolveSituation"
                        }
                    )
                )
                ( May
                    ( MkEvent
                        { getActor = MkParty
                            { getName = "John" }
                        , getAction = "makeMoneyTransfers"
                        }
                    ))
            ]})

λ> recognize autBankExample.aut bankExampleTrace
False
(because of the conflicting clauses)
-}
