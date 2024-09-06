module Main where

import           Base
import qualified Base.Text as T
import qualified Base.ByteString             as BL hiding (null)
import qualified Lam4.Expr.ConcreteSyntax    as CST (Decl)
-- import Lam4.Expr.CEvalAST (CEvalDecl, CEvalExpr(..), DeclF(..))
import           Lam4.Expr.Parser            (parseProgramByteStr)
import           Lam4.Expr.ToConcreteEvalAST (toCEvalDecl)
import qualified Lam4.Expr.ToSimala          as ToSimala (compile, render)
import           Lam4.Parser.Monad           (evalParserFromScratch)
import           Options.Applicative         as Options

data Options =
  MkOptions
    { files   :: [FilePath]
    }

optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> many (strArgument (metavar "CONCRETE SYNTAX JSONs..."))

optionsConfig :: Options.ParserInfo Options
optionsConfig =
  info (optionsDescription <**> helper)
    (  fullDesc
    <> header "Lam4 Backend. This is an *internal*, *unstable* cli that can see breaking changes any time --- use at your own risk!"
    )

-- | This will be wired up to an evaluator instead of just the parser in the near future
main :: IO ()
main = do
  options <- Options.execParser optionsConfig
  if null options.files
    then do
      hPutStrLn stderr "Lam4 Backend: no input files given; use --help for help"
    else do
      cstDecls <- parseProgramFiles options.files
      -- TODO: Use string interpolation...
      pPrint "------- CST -------------" 
      pPrint cstDecls
      pPrint "-------- Simala ---------"
      let smDecls = ToSimala.compile $ map toCEvalDecl cstDecls
      pPrint $ ToSimala.render smDecls


parseProgramFile :: FilePath -> IO [CST.Decl]
parseProgramFile file = do
  bsFile <- BL.readFile file
  case evalParserFromScratch . parseProgramByteStr $ bsFile of
    Left err       -> error ("Parse error:\n" <> ppShow err)
    Right cstDecls -> pure cstDecls

parseProgramFiles :: [FilePath] -> IO [CST.Decl]
parseProgramFiles = fmap concat <$> traverse (\f -> do pPrint f; parseProgramFile f)
             