module Lam4.Parser.Type (
    CSTParser
  , CSTParserError
  , CSTParserState
  , Env            
  ) where

import Base
import Base.IntMap (IntMap)
-- import Lam4.Expr.Name (Name(..))
import Lam4.Expr.ConcreteSyntax

import Control.Monad.Identity()
import Control.Monad.Except()
import Control.Monad.Reader()

-- | Env for identifier Names
newtype Env = MkEnv (IntMap Expr)
  deriving newtype (Eq, Show)

data CSTParserState = MkCSTParserState { env :: Env
                                       , maxUnique :: !Int }
  deriving stock (Show, Generic)

-- | CST means 'Concrete Syntax Parser'
type CSTParser :: Type -> Type
newtype CSTParser a =
  MkCSTParser (CSTParserState -> (Either CSTParserError a, CSTParserState))
    deriving
    (Functor, Applicative, Monad, MonadState CSTParserState, MonadError CSTParserError)
    via  ExceptT CSTParserError (StateT CSTParserState Identity)

type CSTParserError = String