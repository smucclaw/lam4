{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Lam4.Parser.Monad
  (
    -- parseNodeObject
    runParser
  -- , evalParser
  , initialParserState

  -- * JSON-related operations
  , (.:)
  , (.:?)
  , parseFieldWith

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
import qualified Base.Aeson         as A
import           Base.Map           as M
import           Control.Monad.Base
import           Lam4.Expr.Name     (Unique)
import           Lam4.Parser.Type


initialParserState :: ParserState
initialParserState = MkParserState emptyEnv 0

-- parseNodeObject ::
--   ParserError
--   -> Parser a
--   -> (A.Object -> ParserState)
--   -> A.Value -> AesonParser a
-- parseNodeObject errorStr parser restOfParserState = A.withObject errorStr (evalParserWithObj parser restOfParserState)

-- -- TODO: Figure out how to anti-unify `runParserWithObj` and `runParser` (maybe with type applications?)
-- evalParserWithObj :: Parser a -> (A.Object -> ParserState) -> (A.Object -> AesonParser a)
-- evalParserWithObj (MkParser parser) withRestOfParserState =
--   \object -> fst <$> parser (withRestOfParserState object)

runParser ::
  Parser a
  -> (A.Object -> ParserState)
  -> (A.Object -> AesonParser (a, ParserState))
runParser (MkParser parser) withRestOfParserState =
  \obj -> parser (withRestOfParserState obj)

-- evalParser :: Parser a -> (A.Object -> ParserState) -> (A.Object -> AesonParser a)
-- evalParser mparser withRestOfParserState = fmap fst . runParser mparser withRestOfParserState

{---------------------------
    JSON-related operations
-----------------------------}

(.:) :: (A.FromJSON a) => A.Object -> A.Key -> Parser a
(.:) obj key = liftBase (obj A..: key)

(.:?) :: (A.FromJSON a) => A.Object -> A.Key -> Parser (Maybe a)
(.:?) obj key = liftBase (obj A..:? key)

parseFieldWith :: (A.Value -> Parser b) -> A.KeyMap A.Value -> A.Key -> Parser b
parseFieldWith parse obj key = case A.lookup key obj of
    Nothing -> error $ "key " ++ show key ++ " not found"
    Just val  -> parse val


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
