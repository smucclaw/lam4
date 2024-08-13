{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Lam4.Parser.Monad
  (
    evalParserFromScratch
  , evalParser
  , runParser
  , runMaybeParser
  , defaultInitialParserState

  -- * JSON-related operations
  , (.:)
  , objAtKey
  , getObjectsAtField
  , (.:?)
  , parseFieldWith

  -- * Name / Unique related

  -- , getFresh

  -- * RefPath related
  -- , processRefPath
  , refPathToUnique
  , refPathToMaybeUnique

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
import Base.Aeson (_Object, _Array)
import           Base.Map           as M
import           Control.Monad.Base
import           Lam4.Expr.Name     (Unique)
import           Lam4.Parser.Type

-- | Entrypoint
runParser :: Parser a -> ParserState -> Either String (a, ParserState)
runParser (MkParser parser) = A.parseEither parser

runMaybeParser :: Parser a -> ParserState -> Maybe (a, ParserState)
runMaybeParser (MkParser parser) = A.parseMaybe parser

evalParser :: Parser b -> ParserState -> Either String b
evalParser parser parserState = fst <$> runParser parser parserState

evalParserFromScratch :: Parser b -> Either String b
evalParserFromScratch parser = evalParser parser defaultInitialParserState


defaultInitialParserState :: ParserState
defaultInitialParserState = MkParserState emptyEnv 0

{---------------------------
    JSON-related operations
-----------------------------}

-- | Use this only with non-optional keys/values
objAtKey :: (JoinKinds (IxKind s) A_Prism k, Is k An_AffineFold, Ixed s,  A.AsValue (IxValue s)) => s -> Index s -> A.KeyMap A.Value
objAtKey node field = node ^?! ix field % _Object

getObjectsAtField :: (JoinKinds k1 A_Prism k2, JoinKinds k3 A_Fold k1,  JoinKinds (IxKind s) A_Prism k3, Is k2 A_Fold, Ixed s,  A.AsValue (IxValue s)) => s -> Index s -> [A.Object]
getObjectsAtField node field = node ^.. ix field  % _Array % folded % _Object


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

refPathToMaybeUnique :: RefPath -> Parser (Maybe Unique)
refPathToMaybeUnique refpath = lookupInEnv refpath <$> getEnv


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

-- -- TODO: Figure out how to anti-unify `runParserWithObj` and `runMaybeParser` (maybe with type applications?)
-- evalParserWithObj :: Parser a -> (A.Object -> ParserState) -> (A.Object -> AesonParser a)
-- evalParserWithObj (MkParser parser) withRestOfParserState =
--   \object -> fst <$> parser (withRestOfParserState object)

----------------------


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
