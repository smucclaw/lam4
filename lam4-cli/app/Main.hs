{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Base
import qualified Base.ByteString               as BL
import           Control.Lens.Regex.ByteString (groups, regex)
import           Data.ByteString               as BS hiding (concat, concatMap,
                                                      map, null, putStr)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Cradle
import qualified Lam4.Expr.ConcreteSyntax      as CST (Decl)
import           Lam4.Expr.Parser              (parseProgramByteStr)
import           Lam4.Expr.ToConcreteEvalAST   (cstProgramToConEvalProgram)
import           Lam4.Expr.ToSimala            ()
import qualified Lam4.Expr.ToSimala            as ToSimala
import           Lam4.Parser.Monad             (evalParserFromScratch)
import           Options.Applicative           as Options
import           System.FilePath               ((</>))
import           System.Directory

data FrontendConfig =
  MkFrontendConfig { frontendDir :: FilePath
                   , runner      :: String
                   , args        :: [String]
  }

lam4FrontendDir :: FilePath
lam4FrontendDir = "lam4-frontend"

frontendConfig :: FrontendConfig
frontendConfig = MkFrontendConfig { runner      = "node"
                                  , frontendDir = lam4FrontendDir
                                  , args        = [lam4FrontendDir </> "bin" </> "cli", "toMinimalAst"] }

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
      let cstDecls = concatMap parseCSTByteString frontendCSTJsons
          smDecls = ToSimala.compile . cstProgramToConEvalProgram $ cstDecls
      print "------- CST -------------"
      pPrint cstDecls
      print "-------- Simala exprs ---------"
      createDirectoryIfMissing True "generated"
      T.writeFile ("generated" </> "output.simala") (ToSimala.render smDecls)
      putStr $ T.unpack $ ToSimala.render smDecls
      print "-------------------------------"
      -- TODO: What to do if no explicit Eval?
      _ <- ToSimala.doEvalDeclsTracing options.tracing ToSimala.emptyEnv smDecls
      pure ()

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
