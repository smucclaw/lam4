module Lam4.Parser.Monad
  (
    runParser

  -- * Unique related
  , getFresh

  -- * RefPath related
  , processRefPath
  , refPathToUnique

  -- * Env related
  , getEnv
  , withEnv
  , emptyEnv
  , lookupInEnv
  , extendEnv
  , insertEnv
  )
where

import           Base
import           Base.Map         as M
import           Lam4.Expr.Name   (Unique)
import           Lam4.Parser.Type

initialParserState :: ParserState
initialParserState = MkParserState emptyEnv 0

runParser :: CSTParser a -> (Either ParserError a, ParserState)
runParser (MkParser parser) = parser initialParserState

{--------------------
    Unique related
---------------------}

getFresh :: CSTParser Unique
getFresh = do #maxUnique <%= (+ 1)


{--------------------
    RefPath x Env
---------------------}

{- | Get the Unique corresponding to the RefPath of a `NamedElement` from Langium Parser
Assumes that the NamedElements have been processed -}
refPathToUnique :: RefPath -> CSTParser Unique
refPathToUnique refpath = do
  env <- getEnv
  case lookupInEnv refpath env of
    Nothing -> throwError "the input program is assumed to be well-scoped"
    Just v  -> pure v

processRefPath :: RefPath -> CSTParser ()
processRefPath refpath = do
  env <- getEnv
  unless (M.member refpath env) $
    do
      newUnique <- getFresh
      insertEnv refpath newUnique


{--------------------
    Env Operations
---------------------}

getEnv :: CSTParser Env
getEnv = use #refPathEnv

insertEnv :: RefPath -> Unique -> CSTParser ()
insertEnv refPath uniqueV = do
  env <- getEnv
  let newEnv = M.insert refPath uniqueV env
  assign' #refPathEnv newEnv

lookupInEnv :: RefPath -> Env -> Maybe Unique
lookupInEnv = M.lookup

-- | An empty Env.
emptyEnv :: Env
emptyEnv = M.empty

-- | Second environment wins over first.
extendEnv :: Env -> Env -> Env
extendEnv = flip M.union

-- adapted from Simala's evaluator
withEnv :: Env -> CSTParser a -> CSTParser a
withEnv env m = do
  savedEnv <- getEnv
  assign' #refPathEnv env
  r <- m
  assign' #refPathEnv savedEnv
  pure r
