{-# LANGUAGE QuasiQuotes #-}

module PrinterSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Golden
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax
import           Lam4.Expr.Printer       (printTree)
import           Lam4.Expr.Parser        (parseProgramByteStr)
import           Lam4.Parser.Monad       (evalParserFromScratch)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Base.ByteString       as BL (fromStrict)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Data.Text.Lazy.IO     as TL
import           System.FilePath          ((<.>), (</>))
import           Text.Pretty.Simple    as Pretty (pShowNoColor)
import           Text.RawString.QQ        (r)

goldenGeneric :: Show a => String -> a -> Golden TL.Text
goldenGeneric name output_ = Golden
  { output = Pretty.pShowNoColor output_
  , encodePretty = TL.unpack
  , writeToFile = TL.writeFile
  , readFromFile = TL.readFile
  , goldenFile =  testPath <.> "expected"
  , actualFile = Just (testPath <.> "actual")
  , failFirstTime = False
  }
  where
    testPath = "test" </> "testdata" </> "golden" </> "PrinterSpec" </> name

spec :: Spec
spec = do
  test "predicate, no givens" predNoGivens "pred-no-givens"

  where
    test :: String -> String -> String -> Spec
    test desc declStr fname = do
      let decls :: [Decl] = parseCSTString declStr
          printedDecls = printTree <$> decls
      it desc $ goldenGeneric fname printedDecls


predNoGivens :: String
predNoGivens = [r|
  DECIDE `pred blah blah`
  // use the backtick syntax when you want an identifier with spaces
  IF True|]


parseCSTString :: String -> [Decl]
parseCSTString str =
  case evalParserFromScratch . parseProgramByteStr . BL.fromStrict . BS.pack $ str of
    Left err       -> error ("Parse error:\n" <> show err)
    Right cstDecls -> cstDecls
