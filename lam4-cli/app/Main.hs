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

import           Cradle
import qualified Lam4.Expr.ConcreteSyntax      as CST (Decl)
import           Lam4.Expr.Parser              (parseProgramByteStr)
import           Lam4.Expr.ToConcreteEvalAST   (cstProgramToConEvalProgram)
import           Lam4.Expr.ToSimala            (SimalaProgram)
import qualified Lam4.Expr.ToSimala            as ToSimala (compile, render)
import           Lam4.Parser.Monad             (evalParserFromScratch)
import           Options.Applicative           as Options
import           System.FilePath               ((</>))

data FrontendConfig =
  MkFrontendConfig { frontendDir :: FilePath
                   , runner      :: String
                   , args        :: [String]
  }

lam4_frontend_dir :: FilePath
lam4_frontend_dir = "lam4-frontend"

frontendConfig :: FrontendConfig
frontendConfig = MkFrontendConfig { runner      = "node"
                                  , frontendDir = lam4_frontend_dir
                                  , args        = [lam4_frontend_dir </> "bin" </> "cli", "toMinimalAst"] }

-- TODO: Think about exposing a tracing option?
data Options =
  MkOptions
    { files   :: [FilePath]
    }

optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> many (strArgument (metavar ".l4 FILES..."))

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
      print "-------- Simala ---------"
      putStr $ T.unpack $ ToSimala.render smDecls

getCSTJsonFromFrontend :: FrontendConfig -> [FilePath] -> IO [ByteString]
getCSTJsonFromFrontend config files = do
  let argsForFrontendCLI = config.args <> files
  ret@(exitCode :: ExitCode, StdoutRaw rawstdout, StderrRaw err) <- run $ cmd config.runner
    & addArgs argsForFrontendCLI
  case exitCode of
    ExitFailure _ ->
      error (ppShow ret)
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
