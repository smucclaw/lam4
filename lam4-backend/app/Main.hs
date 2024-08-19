module Main where

import           Base
import qualified Base.ByteString     as BL hiding (null)
import           Lam4.Expr.Parser    (parseProgramByteStr)
import           Lam4.Parser.Monad   (evalParserFromScratch)
import           Options.Applicative as Options

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
    else do parseProgramFiles options.files

parseProgramFile :: FilePath -> IO ()
parseProgramFile file = do
  bsFile <- BL.readFile file
  case evalParserFromScratch . parseProgramByteStr $ bsFile of
    Left err  -> pPrint err
    Right cst -> pPrint cst

parseProgramFiles :: [FilePath] -> IO ()
parseProgramFiles inputFiles = do
  forM_ inputFiles (\f -> do
    pPrint f
    parseProgramFile f)
    