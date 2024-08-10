{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lam4.Parser.Monad
  (
    parseNodeObject
  , runParser

  -- * JSON-related operations
  , (.:)
  , (.:?)

  -- * Unique related
  -- , getFresh

  -- * RefPath related
  -- , processRefPath
  , refPathToUnique

  -- * Env related
  , getEnv
  , setEnv
  , emptyEnv
  , lookupInEnv
  , extendEnv
  -- , withEnv
  -- , insertEnv
  )
where

import           Base
import qualified Base.Aeson       as A
import           Base.Map         as M
import           Lam4.Expr.Name   (Unique)
import           Lam4.Parser.Type


makeInitialParserState :: A.Object -> ParserState
makeInitialParserState obj = MkParserState emptyEnv 0 obj

parseNodeObject ::
  ParserError
  -> Parser a
  -> (A.Object -> ParserState)
  -> A.Value -> AesonParser a
parseNodeObject errorStr parser initialRestOfParserState = A.withObject errorStr (runParser parser initialRestOfParserState)

runParser :: Parser a -> (A.Object -> ParserState) -> (A.Object -> AesonParser a)
runParser (MkParser parser) mkParserState = \nodeObj -> fst <$> parser (mkParserState nodeObj)


{---------------------------
    JSON-related operations
-----------------------------}

-- TODO: Refactor to remove redundancy

(.:) :: (A.FromJSON a) => A.Object -> A.Key -> Parser a
(.:) obj key = MkParser $
  \s -> fmap (, s) (obj A..: key)

(.:?) :: (A.FromJSON a) => A.Object -> A.Key -> Parser (Maybe a)
(.:?) obj key = MkParser $
  \s -> fmap (, s) (obj A..:? key)

explicitParseField :: (A.Value -> Parser a) -> A.Object -> A.Key -> Parser a
explicitParseField p obj key = undefined --TODO

{--------------------
    RefPath x Env
---------------------}

{- | Get the Unique corresponding to the RefPath of a `NamedElement` from Langium Parser
Assumes that the NamedElements have been processed -}
refPathToUnique :: RefPath -> Parser Unique
refPathToUnique refpath = do
  env <- getEnv
  case lookupInEnv refpath env of
    Nothing -> error "the input program is assumed to be well-scoped"
    Just v  -> pure v

-- processRefPath :: RefPath -> CSTParser ()
-- processRefPath refpath = do
--   env <- getEnv
--   unless (M.member refpath env) $
--     do
--       newUnique <- getFresh
--       insertEnv refpath newUnique


{--------------------
    Env Operations
---------------------}

getEnv :: Parser Env
getEnv = use #refPathEnv

-- | Makes an env from the supplied AssocList and sets the monad's Env to that
setEnv :: [(RefPath, Unique)] -> Parser ()
setEnv assocList = do
  let newEnv = M.fromList assocList
  assign' #refPathEnv newEnv
  assign' #maxUnique (length assocList)


lookupInEnv :: RefPath -> Env -> Maybe Unique
lookupInEnv = M.lookup

-- | An empty Env.
emptyEnv :: Env
emptyEnv = M.empty

-- | Second environment wins over first.
extendEnv :: Env -> Env -> Env
extendEnv = flip M.union


-----------------------------------------------------------
-- Aug 10: Commenting out stuff that I'm not sure is needed
-----------------------------------------------------------

-- insertEnv :: RefPath -> Unique -> CSTParser ()
-- insertEnv refPath uniqueV = do
--   env <- getEnv
--   let newEnv = M.insert refPath uniqueV env
--   assign' #refPathEnv newEnv

-- adapted from Simala's evaluator
-- withEnv :: Env -> CSTParser a -> CSTParser a
-- withEnv env m = do
--   savedEnv <- getEnv
--   assign' #refPathEnv env
--   r <- m
--   assign' #refPathEnv savedEnv
--   pure r

{--------------------
    Unique related
---------------------}

-- TODO: may not need this
-- getFresh :: CSTParser Unique
-- getFresh = do #maxUnique <%= (+ 1)
