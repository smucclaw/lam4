{-# LANGUAGE RecordWildCards #-}

{-
The key definitions and operations here are copied / adapted from this blog post:
https://iagoleal.com/posts/automata-monads/
-}

module Automata where

import Data.Foldable          (foldlM)
import Control.Monad.Identity (Identity(..))


{- | Finite automaton with state `s`, alphabet `a` and a monadic context `m`.
    The context `m` represents what effects our automaton is capable of. 
    The type parameters `s` and `a` are assumed to represent finite sets.
-}
data Automaton m s a = Automaton
  { initial    :: s               -- ^ Initial State
  , transition :: s -> a -> m s   -- ^ Change state with a context.
  , accepting  :: s -> Bool       -- ^ Accepting subset as a predicate.
  }

run :: Monad m => Automaton m s a -> [a] -> m s
run Automaton{..} = foldlM transition initial

-- | If after running the automaton, it ends in an accepting state, we say that it recognizes the input.
recognize :: (Finite s, Monad m, Context m) => Automaton m s a -> [a] -> Bool
recognize aut@Automaton{..} = possible accepting . run aut

class Context m where
  possible :: Finite s => (s -> Bool) -> (m s -> Bool)

class (Eq a, Ord a) => Finite a

-- | Deterministic Finite Automaton.
type DFA = Automaton Identity

instance Context Identity where
  possible pred (Identity s) = pred s


-- -- | Non-deterministic Finite Automaton.
-- type NFA = Automaton []

-- instance Context [] where
--  possible :: Finite s => (s -> Bool) -> [s] -> Bool
--  possible = any