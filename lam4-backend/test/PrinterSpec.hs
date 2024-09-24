{-# LANGUAGE QuasiQuotes #-}

module PrinterSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Golden
import           Lam4.Main               (getCSTJsonFromFrontendNoFail, parseCSTByteString, FrontendConfig(..))
import           Lam4.Expr.Printer       (printTree)
import           Control.Monad           (forM_)
import           Data.List               (intercalate)
import qualified Data.Text.Lazy        as TL
import qualified Data.Text.Lazy.IO     as TL
import           System.FilePath          ((<.>), (</>), takeBaseName)
import           System.Directory         (listDirectory)

goldenGeneric :: String -> String -> Golden TL.Text
goldenGeneric name output_ = Golden
  { output = TL.pack output_
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
  files <- runIO $ listDirectory examplesDir
  forM_ files $ \file -> do
    frontendCSTJsons <- runIO $ getCSTJsonFromFrontendNoFail frontendConfig [examplesDir </> file]
    let decls = concatMap parseCSTByteString frontendCSTJsons
        fname = takeBaseName file
        descr = "Testing " <> fname
        printedDecls = if null decls
          then show frontendCSTJsons
          else intercalate "\n\n" $ fmap printTree decls
    testGolden descr fname printedDecls
  where
    examplesDir = "../examples"

testGolden :: String -> String -> String -> Spec
testGolden desc fname expected = it desc $ goldenGeneric fname expected

lam4_frontend_dir :: FilePath
lam4_frontend_dir = "../lam4-frontend"

frontendConfig :: FrontendConfig
frontendConfig = MkFrontendConfig { runner      = "node"
                                  , frontendDir = lam4_frontend_dir
                                  , args        = [lam4_frontend_dir </> "bin" </> "cli", "toMinimalAst"] }

