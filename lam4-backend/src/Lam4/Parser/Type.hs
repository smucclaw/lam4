module Lam4.Parser.Type where

import Base
import qualified Base.IntMap as IM
-- import Lam4.Expr.Name (Name(..))
import Lam4.Expr.ConcreteSyntax

import Control.Monad.Identity()
import Control.Monad.Except()
import Control.Monad.Reader()

-- | Env for identifier Names
type Env = IM.IntMap Expr

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