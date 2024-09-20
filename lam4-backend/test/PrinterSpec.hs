{-# LANGUAGE QuasiQuotes #-}

module PrinterSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Golden
import           Lam4.Expr.ConcreteSyntax (Decl)
import           Lam4.Expr.Printer       (printTree)
import           Lam4.Expr.Parser        (parseProgramByteStr)
import           Lam4.Parser.Monad       (evalParserFromScratch)
import           Control.Monad           (forM_)
import           Data.List               (intercalate)
import qualified Data.ByteString.Char8 as BS (readFile)
import qualified Base.ByteString       as BL (fromStrict)
import qualified Data.Text.Lazy        as TL
import qualified Data.Text.Lazy.IO     as TL
import           System.FilePath          ((<.>), (</>), takeBaseName)
import           System.Directory         (listDirectory)
import           Text.Pretty.Simple    as Pretty (pShowNoColor)

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
  files <- runIO $ listDirectory "../examples"
  forM_ files $ \file -> do
    let fpath = "../examples/" <> file
    decls <- runIO $ parseProgramFile fpath
    let fname = takeBaseName file
        descr = "Testing " <> fname
        printedDecls = intercalate "\n" $ fmap printTree decls
    testGolden descr fname printedDecls

testGolden :: String -> String -> String -> Spec
testGolden desc fname expected = it desc $ goldenGeneric fname expected

parseProgramFile :: FilePath -> IO [Decl]
parseProgramFile file = do
  bs <- BS.readFile file
  case evalParserFromScratch . parseProgramByteStr . BL.fromStrict $ bs of
    Left err       -> error ("Parse error:\n" <> show err)
    Right cstDecls -> pure cstDecls

