module Main where

import           Base
import qualified Base.ByteString             as BL
import qualified Builder
import qualified Lam4.Expr.ConcreteSyntax    as CST (Decl)
import           Lam4.Expr.Parser            (parseProgramByteStr)
import           Lam4.Expr.ToConcreteEvalAST (cstProgramToConEvalProgram)
import           Lam4.Expr.ToSimala          (SimalaProgram)
import qualified Lam4.Expr.ToSimala          as ToSimala (compile, render)
import           Lam4.Parser.Monad           (evalParserFromScratch)
import           Options.Applicative

data Command
  = Build
  | Compile [FilePath]
  | Evaluate [FilePath]
  deriving (Show, Eq)

commandParser :: Parser Command
commandParser =
  subparser
    (command
       "compile"
       (info
          (Compile <$> many (strArgument (metavar "CONCRETE SYNTAX JSONs...")))
          (progDesc "Compile Lam4 programs to Simala"))
  -- <> command "build" (info (pure Build) (progDesc "Build the Lam4 backend"))
       <> command
            "eval"
            (info
               (Evaluate
                  <$> many (strArgument (metavar "CONCRETE SYNTAX JSONs...")))
               (progDesc "Evaluate Lam4 programs")))

main :: IO ()
main = do
  cmd <- execParser (info (commandParser <**> helper) fullDesc)
  case cmd of
    Build -> undefined
    Compile files -> do
      smDecls <- parseAndCompile files
      pPrint $ ToSimala.render smDecls
    Evaluate files -> undefined
      -- TODO: Add entrypoint eval

parseAndCompile :: [FilePath] -> IO SimalaProgram
parseAndCompile files = do
  cstDecls <- parseProgramFiles files
  print "------- CST -------------"
  pPrint cstDecls
  print "-------- Simala ---------"
  let smDecls = ToSimala.compile . cstProgramToConEvalProgram $ cstDecls
  pure smDecls

parseProgramFile :: FilePath -> IO [CST.Decl]
parseProgramFile file = do
  bsFile <- BL.readFile file
  case evalParserFromScratch . parseProgramByteStr $ bsFile of
    Left err       -> error ("Parse error:\n" <> ppShow err)
    Right cstDecls -> pure cstDecls

parseProgramFiles :: [FilePath] -> IO [CST.Decl]
parseProgramFiles =
  fmap concat
    <$> traverse
          (\f -> do
             pPrint f
             parseProgramFile f)
