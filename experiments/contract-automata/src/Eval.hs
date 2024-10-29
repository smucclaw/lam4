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

residualToTransition :: ([Clause] -> Trace -> [Clause]) -> (CAState -> Trace -> Identity CAState)
residualToTransition residualFn = \(MkCAState clauses) trace -> Identity $ MkCAState $ residualFn clauses trace

------------------------
  --- Residual
------------------------

-- | Residual for clause*s* / contracts (that can have no clauses)
residual :: [Clause] -> Trace -> [Clause]
residual = synchronouslyCompose residual'

{- | See CA p.32 and https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell/blob/b763318a826fef320fe6773b4fe0b6b095112027/UnknownDL.hs#L75 -}
synchronouslyCompose :: (Clause -> Trace -> Clause) -> [Clause] -> Trace -> [Clause]
synchronouslyCompose clauseResidual clauses trace = fmap (`clauseResidual` trace) clauses
  -- synchronous composition corresponds to conjunction over clauses

-- | Residual at the level of *clauses* (as opposed to contracts / sequences of clauses)
residual' :: Clause -> Trace -> Clause
residual' = flip flippedResidual'
  where
    flippedResidual' :: Trace -> Clause -> Clause
    flippedResidual' trace = \case
        Top               -> Top
        Bottom            -> Bottom
        Must event        ->
          if trace `includesEvent` event
          then Top
          else Bottom
        May _             -> Top
        Shant event       ->
          if trace `includesEvent` event
          then Bottom
          else Top
        If guard clause   ->
          if trace `satisfiesGuard` guard
          then clause
          else Top

------------------------
  --- Trace operators
------------------------

satisfiesGuard :: Trace -> Guard -> Bool
satisfiesGuard trace = \case
  GDone event -> trace `includesEvent` event
  GNot guard  -> satisfiesGuard trace guard
  GTrue       -> True
  GFalse      -> False

includesEvent :: Trace -> Event -> Bool
includesEvent (MkTrace events) event = event `elem` events
