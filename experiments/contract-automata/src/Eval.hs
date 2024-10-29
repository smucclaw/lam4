{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Eval where

import           Automata
import           ContractAutomaton
import           Syntax
-- import Data.Coerce (coerce)
import           Control.Monad.Identity (Identity (..))

{----------------------
  Resources / See also
-----------------------

* https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell

SCL ("SCL: A domain-specific language for normative texts with timing constraints"):
  * Operational semantics for SCL: https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Semantics/Orig.hs
  * https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Semantics/Common.hs

-}

-- | Smart constructor for making a contract automaton. Uses the `residual` function defined in Eval.hs
mkContractAut :: CAState -> ContractAutomaton
mkContractAut initial = MkContractAut (Automaton initial trans acceptingPred)
  where
    trans = residualToTransition residual

residualToTransition :: ([Clause] -> Event -> [Clause]) -> (CAState -> Event -> Identity CAState)
residualToTransition residualFn = \(MkCAState clauses) trace -> Identity $ MkCAState $ residualFn clauses trace

------------------------
  --- Residual
------------------------

-- | Residual for clause*s* / contracts (that can have no clauses)
residual :: [Clause] -> Event -> [Clause]
residual = synchronouslyCompose residual'

{- | See CA p.32 and https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell/blob/b763318a826fef320fe6773b4fe0b6b095112027/UnknownDL.hs#L75 -}
synchronouslyCompose :: (Clause -> Event -> Clause) -> [Clause] -> Event -> [Clause]
synchronouslyCompose clauseResidual clauses trace = fmap (`clauseResidual` trace) clauses
  -- synchronous composition corresponds to conjunction over clauses

{- | Residual at the level of *clauses* (as opposed to contracts / sequences of clauses).
  NOTE: I depart from the paper by simplifying this to use only one event/action as opposed to a set/list of them
-}
residual' :: Clause -> Event -> Clause
residual' = flip flippedResidual'
  where
    flippedResidual' :: Event -> Clause -> Clause
    flippedResidual' actualEvent = \case
        Top               -> Top
        Bottom            -> Bottom
        Must stateOfAffairs   ->
          if actualEvent `matches` stateOfAffairs
          then Top
          else Bottom
        May _             -> Top
        Shant stateOfAffairs       ->
          if actualEvent `matches` stateOfAffairs
          then Bottom
          else Top
        If guard clause   ->
          if actualEvent `satisfiesGuard` guard
          then clause
          else Top

------------------------
  --- Event operators
------------------------

matches :: Event -> Event -> Bool
actualEvent `matches` hypothesizedStateOfAffairs = actualEvent == hypothesizedStateOfAffairs

satisfiesGuard :: Event -> Guard -> Bool
satisfiesGuard actualEvent = \case
  GDone stateOfAffairs -> actualEvent `matches` stateOfAffairs
  GNot guard  -> not $ actualEvent `satisfiesGuard` guard
  GTrue       -> True
  GFalse      -> False


