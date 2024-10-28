{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Base
import           Base.Aeson                    (encodePretty)
import qualified Base.ByteString               as BL
import           Base.File
import           Control.Lens.Regex.ByteString (groups, regex)
import           Data.ByteString               as BS hiding (concat, concatMap,
                                                      map, null, putStr)
import qualified Data.Text                     as T

import           Cradle
import qualified Lam4.Expr.ConcreteSyntax      as CST (Decl)
import           Lam4.Expr.ExtractProgramInfo
import           Lam4.Expr.Parser              (parseProgramByteStr)
import           Lam4.Expr.ToConcreteEvalAST   (cstProgramToConEvalProgram)
import           Lam4.Expr.ToSimala            ()
import qualified Lam4.Expr.ToSimala            as ToSimala
import qualified Lam4.Render.Render            as Render
import Lam4.Render.Render (NLGConfig(..))
import           Lam4.Parser.Monad             (evalParserFromScratch)
import           Options.Applicative           as Options
import           System.Directory
import           System.FilePath               ((</>))

data FrontendConfig =
  MkFrontendConfig { frontendDir :: FilePath
                   , runner      :: String
                   , args        :: [String]
  }

data SimalaOutputConfig =
  MkSimalaOutputConfig {
    outputDir           :: FilePath
  , programFilename     :: FilePath
  , programInfoFilename :: FilePath
}

lam4FrontendDir :: FilePath
lam4FrontendDir = "lam4-frontend"

frontendConfig :: FrontendConfig
frontendConfig = MkFrontendConfig { runner      = "node"
                                  , frontendDir = lam4FrontendDir
                                  , args        = [lam4FrontendDir </> "bin" </> "cli", "toMinimalAst"] }

-- TODO: Most of the following should be put in, and read from, the .env file
simalaOutputConfig :: SimalaOutputConfig
simalaOutputConfig = MkSimalaOutputConfig {
    outputDir = "generated" </> "simala"
  , programFilename = "output.simala"
  , programInfoFilename = "program_info.json" }

-- TODO: read outputDir in from a .env file
nlgConfig :: NLGConfig
nlgConfig = MkNLGConfig {
    outputDir = "generated" </> "nlg_en"
  , resultFilename = "output.txt"
  , abstractSyntaxFilename = "Lam4.pgf"
  , concreteSyntaxName = "Lam4Eng"
}

-- TODO: Think about exposing a tracing option?
data Options =
  MkOptions
    { tracing :: ToSimala.TraceMode
    , files   :: [FilePath]
    }

-- | Copied from Simala
toTracingMode :: String -> ToSimala.TraceMode
toTracingMode "full"    = ToSimala.TraceFull
toTracingMode "results" = ToSimala.TraceResults
toTracingMode "off"     = ToSimala.TraceOff
toTracingMode _         = ToSimala.TraceFull


optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> (toTracingMode <$> strOption (long "tracing" <> help "Tracing, one of \"off\", \"full\" (default), \"results\"") <|> pure ToSimala.TraceResults)
  <*> many (strArgument (metavar ".l4 FILES..."))

optionsConfig :: Options.ParserInfo Options
optionsConfig =
  info (optionsDescription <**> helper)
    (  fullDesc
    <> header "Lam4 (an experimental variant of the L4 legal DSL)"
    )

main :: IO ()
main = do
  options <- Options.execParser optionsConfig
  if null options.files
    then do
      hPutStrLn stderr "Lam4: no input files given; use --help for help"
    else do
      frontendCSTJsons <- getCSTJsonFromFrontend frontendConfig options.files
      let cstProgram       = concatMap parseCSTByteString frontendCSTJsons
          conEvalProgram = cstProgramToConEvalProgram cstProgram
          simalaProgram  = ToSimala.compile conEvalProgram
          programInfo    = extractProgramInfo conEvalProgram

      -- Create output directory and write files first
      createDirectoryIfMissing True simalaOutputConfig.outputDir
      -- save simala program and program info to disk
      writeFileUtf8 (simalaOutputConfig.outputDir </> simalaOutputConfig.programFilename) (ToSimala.render simalaProgram)
      writeFileLBS (simalaOutputConfig.outputDir </> simalaOutputConfig.programInfoFilename) (encodePretty programInfo)

      -- NLG (put this behind an option later)
      -- TODO: Make a ToNLG monad
      nlgEnv <- Render.makeNLGEnv nlgConfig
      let nlRendering = Render.renderCstProgramToNL nlgEnv cstProgram 
      createDirectoryIfMissing True nlgConfig.outputDir
      writeFileUtf8 (nlgConfig.outputDir </> nlgConfig.resultFilename) nlRendering
      -- TODO: Save a json version with the nlg output as a member

      -- Perform evaluation (if needed)
      _ <- ToSimala.doEvalDeclsTracing options.tracing ToSimala.emptyEnv simalaProgram

      -- Finally print output and signal success
      print "--- CST -----------"
      pPrint cstProgram
      print "--- Simala exprs --------"
      putStr $ T.unpack $ ToSimala.render simalaProgram
      print "-------------------------------"

      putStrLn "---- Natural language (sort of) -----"
      putStr $ T.unpack nlRendering

getCSTJsonFromFrontend :: FrontendConfig -> [FilePath] -> IO [ByteString]
getCSTJsonFromFrontend config files = do
  let argsForFrontendCLI = config.args <> files
  (exitCode :: ExitCode, StdoutRaw rawstdout, StderrRaw err) <- run $ cmd config.runner
    & addArgs argsForFrontendCLI
  case exitCode of
    ExitFailure _ ->
      error ("Frontend parser failed:\n" <> ppShow err)
      -- TODO: Improve the error reporting
    ExitSuccess ->
      pure $ concat $ rawstdout ^.. cstJsonTraversal
        where
          cstJsonTraversal = traversalVL [regex|(?s)<ast_from_frontend>(.*?)</ast_from_frontend>|] % traversalVL groups

parseCSTByteString :: StrictByteString -> [CST.Decl]
parseCSTByteString bs =
  case evalParserFromScratch . parseProgramByteStr . BL.fromStrict $ bs of
    Left err       -> error ("Parse error:\n" <> ppShow err)
    Right cstDecls -> cstDecls


parseProgramFile :: FilePath -> IO [CST.Decl]
parseProgramFile file = do
  bsFile <- BS.readFile file
  pure $ parseCSTByteString bsFile
