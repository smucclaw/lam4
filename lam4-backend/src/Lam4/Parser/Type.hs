{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lam4.Parser.Type (
  -- * Parser related
  Parser(..),
  ParserState(..),
  AesonParser,
  ParserError,
  liftBase,

  -- * RefPath, Env
  RefPath,
  Env,
  -- * RecordLabel related
  RecordLabel,
  RecordLabelEnv,
) where

import           Base
import qualified Base.Aeson             as A
import           Lam4.Expr.Name         (Unique, Name(..))
import           Lam4.Expr.CommonSyntax (RecordLabel)

import           Control.Monad.Base
import           Control.Monad.Except   ()
import           Control.Monad.Identity ()
import           Control.Monad.Reader   ()


{- | JSONPath for Refs

Invariant: The path always starts with the global root element of the json-serialized concrete syntax we're parsing

__Examples:__

@
"#/elements@2"
"#/elements@3/params@0/param"
"#/elements@0"
@
-}
type RefPath = Text

-- | Environment for Parser: map from RefPaths to Uniques/Ints
type Env = Map RefPath Unique

{- | See relevant discussion in Parser.hs -}
type RecordLabelEnv = Map RecordLabel Name

{----------------------------
    ParserState, Parser
-----------------------------}

-- | State for our Parser monad. Does NOT include the input object / value
data ParserState = MkParserState
  { refPathEnv     :: !Env
  , maxUnique      :: !Unique 
    -- ^ maxUnique in @refPathEnv@. TODO: may not need this; we'll see
  , recordLabelEnv :: !RecordLabelEnv
  }
  deriving stock (Show, Generic)

type AesonParser = A.Parser

-- | Concrete Syntax Parser monad, for parsing the concrete syntax json serialized from the Langium parser
type Parser :: Type -> Type
newtype Parser a
  = MkParser (ParserState -> AesonParser (a, ParserState))
  deriving
    (Functor, Applicative, Monad, MonadState ParserState)
    via (StateT ParserState AesonParser)

instance MonadBase AesonParser Parser where
  liftBase :: AesonParser a -> Parser a
  liftBase aesonParser = MkParser $ \s -> fmap (, s) aesonParser


type ParserError = String
