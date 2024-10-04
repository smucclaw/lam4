{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}

module Lam4.Expr.ExtractProgramInfo (ProgramInfo, extractProgramInfo) where

import           Base
-- import           Base.Text()
import           Base.Aeson
import           Lam4.Expr.ConEvalAST as AST
import           Lam4.Expr.Name
-- import Lam4.Expr.CommonSyntax

newtype ProgramInfo = MkProgramInfo { entryPointFunctions :: [EntrypointFunctionInfo] }
  deriving newtype (Eq, Ord, Show)
  deriving stock (Generic)

newtype EntrypointFunctionInfo = MkEntrypointFunctionInfo {
    functionName   :: SimpleName
  -- in the future:
  -- signature :: TypeExpr
}
  deriving newtype (Eq, Ord, Show)
  deriving stock (Generic)


data SimpleName = MkSimpleName {
    textName :: Text
  , unique   :: !Unique
}
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)

instance ToJSON ProgramInfo
instance FromJSON ProgramInfo

instance ToJSON EntrypointFunctionInfo
instance FromJSON EntrypointFunctionInfo

instance ToJSON SimpleName
instance FromJSON SimpleName

extractProgramInfo :: ConEvalProgram -> ProgramInfo
extractProgramInfo program = MkProgramInfo $
  program ^.. folded
          % filtered isToplevelEntrypointFunction
          % to makeEntrypointFunInfo
  where
    makeEntrypointFunInfo :: ConEvalDecl -> EntrypointFunctionInfo
    makeEntrypointFunInfo decl =
      let name = getName decl
      in MkEntrypointFunctionInfo $ MkSimpleName name.name (fromJust name.unique)
      -- there shouldn't be an Entrypoint function with Nothing for unique
      -- (when time permits, just try to remove the Maybe)

    getName :: DeclF expr -> Name
    getName = \case
      NonRec name _  -> name
      Rec    name _  -> name
      _              -> error "impossible"

isToplevelEntrypointFunction :: ConEvalDecl -> Bool
isToplevelEntrypointFunction = \case
  NonRec (MkName _ _ IsEntrypoint) _    -> True
  Rec (MkName _ _ IsEntrypoint) _       -> True
  NonRec (MkName _ _ NotEntrypoint) _   -> False
  Rec (MkName _ _ NotEntrypoint) _      -> False
  Eval{}                                -> False
  DataDecl{}                            -> False
