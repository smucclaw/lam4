{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Eval where

import           Automata
import           ContractAutomaton
import           Syntax
-- import Data.Coerce (coerce)
import           Control.Bool
import           Control.Monad.Identity    (Identity (..))
import           Data.Coerce
import           Data.Containers.ListUtils
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Data.Map.Merge.Strict     as Map
import           Data.List

{----------------------
  Resources / See also
-----------------------

* https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell

SCL ("SCL: A domain-specific language for normative texts with timing constraints"):
  * Operational semantics for SCL: https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Semantics/Orig.hs
  * https://remu.grammaticalframework.org/contracts/time2016/dist/SCL/Semantics/Common.hs

-}

-- | Smart constructor for making a contract automaton. Uses the `residual` function defined in Eval.hs
-- mkContractAut :: CAState -> ContractAutomaton
mkContractAut :: CAState -> ([Clause] -> Event -> [Clause]) -> ContractAutomaton
mkContractAut initial residualFunc = MkContractAut (Automaton initial trans acceptingPred)
  where
    trans = residualToTransition residualFunc

residualToTransition :: ([Clause] -> Event -> [Clause]) -> (CAState -> Event -> Identity CAState)
residualToTransition residualFn = \(MkCAState clauses) event -> Identity $ MkCAState $ residualFn clauses event

----------------------------
  --- Accepting predicate
----------------------------

acceptingPred :: CAState -> Bool
acceptingPred =  noConflictingClauses <&&> clausesAreTopAfterSimplify

noConflictingClauses :: CAState -> Bool
noConflictingClauses (MkCAState clauses) = null $ findEventsThatAppearInConflictingClauses clauses
  where
    findEventsThatAppearInConflictingClauses clauzes = [event | event <- eventsFromClauses clauzes,
                                                          May event `elem` clauses,
                                                          Shant event `elem` clauses] <>
                                                       [event | event <- eventsFromClauses clauzes,
                                                          Must event `elem` clauses,
                                                          Shant event `elem` clauses]
                                                        -- Simplification: not worrying about other kinds of conflicts
                                                        -- Note also that there isn't currently explicit support for omissions as actions


clausesAreTopAfterSimplify :: CAState -> Bool
clausesAreTopAfterSimplify (MkCAState clauses) = all ((== Top) . simplify) clauses

------------------------
  --- Residual
------------------------

-- | Residual for clause*s* / contracts (that can have no clauses)
residualWithoutSimplify :: [Clause] -> Event -> [Clause]
residualWithoutSimplify = synchronouslyCompose residual'

residualWithSimplify :: [Clause] -> Event -> [Clause]
residualWithSimplify = \clauses event -> simplify <$> synchronouslyCompose residual' clauses event

{- | See CA p. 28, 32, and https://github.com/shaunazzopardi/deontic-logic-with-unknowns-haskell/blob/b763318a826fef320fe6773b4fe0b6b095112027/UnknownDL.hs#L75 -}
synchronouslyCompose :: (Clause -> Event -> Clause) -> [Clause] -> Event -> [Clause]
synchronouslyCompose clauseResidual = \clauses event -> fmap (`clauseResidual` event) clauses
  -- synchronous composition corresponds to conjunction over clauses

data Transitions s =
  MkTransitions (Map Event s) s
  deriving (Show, Functor)

data Transitions' =
  MkTransitions' [(Condition, CAState)] CAState

data Condition =
  MkCondition [PosNeg]

data PosNeg =
  Pos Event | Neg Event

topState :: CAState
topState = MkCAState []

bottomState :: CAState
bottomState = MkCAState [Bottom]

-- MkTransitions (Map.fromList [(e1, cs1), (e2, cs2)]) cs3
--
-- would represent three outgoing edges,
-- one would be labelled with e1,
-- the other would be labelled with e2,
-- and one would be saying "anything else"

emptyTransitions :: Transitions CAState
emptyTransitions =
  MkTransitions Map.empty (MkCAState [])

mergeTransitions :: Transitions CAState -> Transitions CAState -> Transitions CAState
mergeTransitions (MkTransitions m1 d1) (MkTransitions m2 d2) =
  MkTransitions
    (Map.merge
      (Map.mapMissing (const (<> d2)))
      (Map.mapMissing (const (d1 <>)))
      (Map.zipWithMatched (const (<>)))
      m1 m2
    ) (d1 <> d2)

allResiduals :: Clause -> Transitions CAState
allResiduals Top                    = MkTransitions Map.empty topState
allResiduals Bottom                 = MkTransitions Map.empty bottomState
allResiduals (Must stateOfAffairs)  =
  MkTransitions (Map.singleton stateOfAffairs topState) bottomState
allResiduals (May _)                = MkTransitions Map.empty topState -- top, following the paper
allResiduals (Shant stateOfAffairs) =
  MkTransitions (Map.singleton stateOfAffairs bottomState) topState
allResiduals (If GTrue clause)      =
  MkTransitions Map.empty (MkCAState [clause])
allResiduals (If guard clause)      =
  MkTransitions (Map.fromList ((\ e -> (e, MkCAState [clause])) <$> eventsSatisfying guard)) topState -- top, following the paper

-- | TODO: this has to be made more clever
eventsSatisfying :: Guard -> [Event]
eventsSatisfying (GDone e) = [e]
eventsSatisfying (GNot _g) = [] -- TODO: this is wrong
eventsSatisfying GTrue     = [] -- this is fine, handled via default
eventsSatisfying GFalse    = []

allResidualsClauses :: [Clause] -> Transitions CAState
allResidualsClauses clauses =
  foldl' mergeTransitions emptyTransitions (allResiduals <$> clauses)

normalise :: [Clause] -> [Clause]
normalise clauses =
  let
    r = filter (/= Top) (nubOrd (sort clauses))
  in
    if Bottom `elem` r
      then [Bottom]
      else r

normaliseState :: CAState -> CAState
normaliseState = coerce normalise

normaliseTransitions :: Transitions CAState -> Transitions CAState
normaliseTransitions (MkTransitions m d) =
  MkTransitions (normaliseState <$> m) (normaliseState d)

locationsFromTransitions :: Transitions CAState -> [CAState]
locationsFromTransitions (MkTransitions m d) =
  nubOrd (d : Map.elems m)

explore :: [CAState] -> [CAState] -> [(CAState, Transitions CAState)]
explore []       _visited = []
explore (s : ss)  visited
  | s `elem` visited      = explore ss visited
  | otherwise             =
    let
      r = normaliseTransitions (allResidualsClauses (coerce s))
    in
      (s, r) : explore (locationsFromTransitions r <> ss) (s : visited)

buildAutomaton :: CAState -> Aut CAState
buildAutomaton initial =
  MkAut (explore [initial] [])

data Aut s =
  MkAut { unAut :: [(s, Transitions s)] }
  deriving (Functor)

instance Show s => Show (Aut s) where
  show (MkAut ts) =
    unlines (map renderState ts)

renderState :: Show s => (s, Transitions s) -> String
renderState (s, ts) =
  "State " <> show s <> ":\n" <> unlines (map ("  " <>) (renderTransitions ts))

renderTransitions :: Show s => Transitions s -> [String]
renderTransitions (MkTransitions m d) =
  let
    xs = Map.toList m
  in
    map (\ (e, s) -> show e <> " -> " <> show s) xs ++ ["default -> " <> show d]

renameAutomaton :: Aut CAState -> Aut Int
renameAutomaton original@(MkAut o) =
  let
    stateMap :: Map CAState Int
    stateMap = Map.fromList (zip (fst <$> o) [0 ..])
  in
    (stateMap Map.!) <$> original

{- | Residual at the level of *clauses* (as opposed to contracts / sequences of clauses).
  NOTE: I depart from the paper by simplifying this to use only one event/action as opposed to a set/list of them
-}
residual' :: Clause -> (Event -> Clause)
residual' = flip flippedResidual'
  where
    flippedResidual' :: Event -> Clause -> Clause
    flippedResidual' actualEvent = \case
        Top                   -> Top
        Bottom                -> Bottom
        Must stateOfAffairs   ->
          if actualEvent `matches` stateOfAffairs
          then Top
          else Bottom
        mc@(May _)            -> mc -- Top in the paper
        Shant stateOfAffairs  ->
          if actualEvent `matches` stateOfAffairs
          then Bottom
          else Top
        cif@(If guard clause)  ->
          if actualEvent `satisfiesGuard` guard
          then clause
          else cif -- I think the paper would just return Top, but if you need that behavior, can just use residualWithSimplify

-- | Seems natural to compose this with residual' for some (but not all) usecases
simplify :: Clause -> Clause
simplify = \case
  If _ _      -> Top
  May _       -> Top
  clause      -> clause

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
