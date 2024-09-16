{-# LANGUAGE QuasiQuotes #-}

module Lam4.Render.Render where

import qualified Base.Text                as T
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax
import           Lam4.Expr.Name           (Name(..))
import           Data.String.Interpolate  (i)
import           Control.Lens             ((&), (%~))
import           Control.Lens.Regex.Text  (match, regex)
import           Paths_lam4_backend       (getDataFileName)

-- GF-related stuff
import           Lam4.Render.Lam4Gf
import           PGF

-- Loosely copied from dsl/…/natural4
data NLGEnv = NLGEnv
  { gfGrammar :: PGF
  , gfLang :: Language
--  , gfParse :: Type -> T.Text -> [Expr]
  , gfLin :: PGF.Expr -> T.Text
  }

gfPath :: String -> String
gfPath x = [i|gf-grammar/#{x}|]

myNLGEnv :: IO NLGEnv
myNLGEnv = do
  grammarFile <- getDataFileName $ gfPath "Lam4.pgf"
  gr <- readPGF grammarFile
  let lang = getLang "Lam4Eng" gr
      -- myParse typ txt = parse gr eng typ (Text.unpack txt)
      myLin = postprocessText . T.pack . linearize gr lang
  pure $ NLGEnv gr lang myLin
  where
    postprocessText :: T.Text -> T.Text
    postprocessText = newlines . tabs . rmBIND
    rmBIND :: T.Text -> T.Text
    rmBIND input = input & [regex|\s+&\+\s+|] . match %~ const ""

    tabs :: T.Text -> T.Text
    tabs = T.map (\c -> if c == '°' then ' ' else c)

    newlines :: T.Text -> T.Text
    newlines = T.map (\c -> if c == '∞' then '\n' else c)

    getLang :: String -> PGF -> Language
    getLang str gr =
      case (readLanguage str, languages gr) of
        (Just l, langs)  -- Language looks valid, check if in grammar
          -> if l `elem` langs
                then l
                else error [i|Render.getLang: #{str} not found among #{langs}|]
        (Nothing, langs)
          -> error [i|Render.getLang: #{str} not a valid language. (GF grammar contains #{langs}.)|]

renderNL :: NLGEnv -> Decl -> T.Text
renderNL env = gfLin env . gf . parseDecl


parseDecl :: Decl -> GTypeDecl
parseDecl = \case
  TypeDecl name typedecl ->
    GMkTypeDecl GNoMetadata (getName name) (parseRows typedecl)
  x -> error [i|parseDecl: not yet implemented #{x}|]

getName :: Name -> GString
getName (MkName name _) = GString $ T.unpack name

parseRows :: TypeDecl -> GListRowTypeDecl
parseRows = \case
  RecordDecl rowtypedecls _parents _metadata ->
    GListRowTypeDecl (parseRow <$> rowtypedecls)

parseRow :: RowTypeDecl -> GRowTypeDecl
parseRow rtd = GMkRowDecl GNoMetadata $ getName rtd.name
  -- where
  --   metadata = (GMkMetadata . GString . T.unpack) <$> rtd.metadata.description


{-
    cst :: CST.Decl = TypeDecl
      (MkName "Lottery" 1)
      (RecordDecl
        [ MkRowTypeDecl
            (MkName "total_jackpot" 2)
            (BuiltinType BuiltinTypeInteger)
            (MkRowMetadata $ Just "how much can be won from the jackpot")

        , MkRowTypeDecl
            (MkName "`tax deductible status`" 4)
            (BuiltinType BuiltinTypeBoolean)
            (MkRowMetadata $ Just "whether buying tickets from this lottery is tax deductible")

        ]
        []
        (RecordDeclMetadata Transparent (Just "game where you lose money"))
      )
-}

