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

  , throwError

  -- * JSON-related operations
  , (.:)
  , objAtKey
  , getObjectsAtField
  , (.:?)

  -- * RefPath related
  , refPathToUnique
  , refPathToMaybeUnique

  -- * Env related
  , getEnv
  , setEnv
  , emptyEnv
  , lookupInEnv

  -- * NodeNameStatusEnv related
  , getNodeNameStatusEnv
  , lookupReferentNodeStatus
  , setNodeNameStatusEnv

  -- * RecordLabelEnv related
  , emptyRecordLabelEnv
  , setRecordLabelEnv
  , lookupRecordLabel
  )
where

import           Base               hiding (throwError)
import           Base.Aeson         (_Array, _Object)
import qualified Base.Aeson         as A
import           Base.Map           as M
import           Control.Monad.Base
import           Lam4.Expr.Name
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
defaultInitialParserState = MkParserState emptyEnv 0 emptyRecordLabelEnv emptyLangiumNodeNameStatusEnv

{-| This is the unsafe @error@.
But that's OK, because any errors here are programmer errors,
since user errors would already have been caught by the upstream
parser, validator, scoper in the Langium framework.
 -}
throwError :: [Char] -> Parser a
throwError = error

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

-- parseFieldWith :: (A.Value -> Parser b) -> A.KeyMap A.Value -> A.Key -> Parser b
-- parseFieldWith parse obj key = case A.lookup key obj of
--     Nothing -> error $ "key " ++ show key ++ " not found"
--     Just val  -> parse val


{--------------------
    RefPath x Env
---------------------}

{- | Get the Unique corresponding to the RefPath of a `NamedElement` from Langium Parser
Assumes that the NamedElements have been processed -}
refPathToUnique :: RefPath -> Parser Unique
refPathToUnique refpath = do
  env <- getEnv
  case lookupInEnv refpath env of
    Nothing -> throwError "the input program is assumed to be well-scoped"
    Just v  -> pure v

refPathToMaybeUnique :: RefPath -> Parser (Maybe Unique)
refPathToMaybeUnique refpath = lookupInEnv refpath <$> getEnv

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

-- | An empty Env.
emptyEnv :: Env
emptyEnv = M.empty

lookupInEnv :: RefPath -> Env -> Maybe Unique
lookupInEnv = M.lookup

{-----------------------------------
    NodeNameStatusEnv Operations
------------------------------------}

getNodeNameStatusEnv :: Parser ReferentStatusEnv
getNodeNameStatusEnv = use #referentStatusEnv

setNodeNameStatusEnv :: [(Unique, ReferentStatus)] -> Parser ()
setNodeNameStatusEnv assocList = assign' #referentStatusEnv (M.fromList assocList)

lookupReferentNodeStatus :: Unique -> Parser ReferentStatus
lookupReferentNodeStatus unique = do
  env <- getNodeNameStatusEnv
  case M.lookup unique env of
    Nothing             -> throwError "Every unique in the program should have had its node name status registered!"
    Just nodeNameStatus -> pure nodeNameStatus

emptyLangiumNodeNameStatusEnv :: ReferentStatusEnv
emptyLangiumNodeNameStatusEnv = M.empty

{-------------------------------
    RecordLabelEnv Operations
--------------------------------}

getRecordLabelEnv :: Parser RecordLabelEnv
getRecordLabelEnv = use #recordLabelEnv

emptyRecordLabelEnv :: RecordLabelEnv
emptyRecordLabelEnv = M.empty

setRecordLabelEnv :: [(RecordLabel, Name)] -> Parser ()
setRecordLabelEnv assocList = do
  assign' #recordLabelEnv (M.fromList assocList)

lookupRecordLabel :: RecordLabel -> Parser Name
lookupRecordLabel label = do
  reclabelEnv <- getRecordLabelEnv
  case M.lookup label reclabelEnv of
    Nothing -> throwError "[lookupRecordLabel error] the input program is assumed to be well-scoped and well-typed"
    Just v  -> pure v


{-----------------------------------------------------------
  Aug 10: The following isn't currently needed
          because I'm currently making a dictionary of the refpaths at the start,
          instead of doing it *as* I parse the .json cst
-----------------------------------------------------------}
-- -- | Second environment wins over first.
-- extendEnv :: Env -> Env -> Env
-- extendEnv = flip M.union

-- processRefPath :: RefPath -> Parser ()
-- processRefPath refpath = do
--   env <- getEnv
--   unless (M.member refpath env) $
--     do
--       newUnique <- getFresh
--       insertEnv refpath newUnique

------------

-- insertEnv :: RefPath -> Unique -> Parser ()
-- insertEnv refPath uniqueV = do
--   env <- getEnv
--   let newEnv = M.insert refPath uniqueV env
--   assign' #refPathEnv newEnv

-- adapted from Simala's evaluator
-- withEnv :: Env -> Parser a -> Parser a
-- withEnv env m = do
--   savedEnv <- getEnv
--   assign' #refPathEnv env
--   r <- m
--   assign' #refPathEnv savedEnv
--   pure r

{--------------------
    Unique related
---------------------}

-- getFresh :: Parser Unique
-- getFresh = do #maxUnique <%= (+ 1)
