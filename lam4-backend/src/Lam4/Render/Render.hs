{-# LANGUAGE QuasiQuotes, OverloadedRecordDot #-}

module Lam4.Render.Render where

import qualified Base.Text                as T
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax
import qualified Lam4.Expr.Name           as N (Name(..), ReferentStatus(..))
import           Data.String.Interpolate  (i)
import           Control.Lens             ((&), (%~))
import           Control.Lens.Regex.Text  (match, regex)
import           Paths_lam4_backend       (getDataFileName)

-- GF-related stuff
import           Lam4.Render.Lam4Gf
import qualified PGF

-- Loosely copied from dsl/…/natural4
data NLGEnv = NLGEnv
  { gfGrammar :: PGF.PGF
  , gfLang :: PGF.Language
--  , gfParse :: Type -> T.Text -> [Expr]
  , gfLin :: PGF.Expr -> T.Text
  }

gfPath :: String -> String
gfPath x = [i|gf-grammar/#{x}|]

myNLGEnv :: IO NLGEnv
myNLGEnv = do
  grammarFile <- getDataFileName $ gfPath "Lam4.pgf"
  gr <- PGF.readPGF grammarFile
  let lang = getLang "Lam4Eng" gr
      -- myParse typ txt = parse gr eng typ (Text.unpack txt)
      myLin = postprocessText . T.pack . PGF.linearize gr lang
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

    getLang :: String -> PGF.PGF -> PGF.Language
    getLang str gr =
      case (PGF.readLanguage str, PGF.languages gr) of
        (Just l, langs)  -- Language looks valid, check if in grammar
          -> if l `elem` langs
                then l
                else error [i|Render.getLang: #{str} not found among #{langs}|]
        (Nothing, langs)
          -> error [i|Render.getLang: #{str} not a valid language. (GF grammar contains #{langs}.)|]

renderNL :: NLGEnv -> Decl -> T.Text
renderNL env = gfLin env . gf . parseDecl


parseDecl :: Decl -> GS
parseDecl = \case
  DataDecl name typedecl -> GTypeDeclS $ parseTypeDecl name typedecl
  Rec name expr -> GExprS $ parseExpr name expr
  Eval expr -> GExprS $ parseExpr noName expr
  x -> error [i|parseDecl: not yet implemented #{x}|]

noName :: N.Name
noName = N.MkName mempty Nothing N.NotEntrypoint

parseName :: N.Name -> GName
parseName = GMkName . GString . T.unpack . N.name

parseUnaOp :: UnaryOp -> GUnaryOp
parseUnaOp = \case
  Not -> GNot
  UnaryMinus -> GUnaryMinus
  Floor -> GFloor
  Ceiling -> GCeiling
  IntegerToFraction -> GIntegerToFraction

parseBinOp :: BinOp -> GBinOp
parseBinOp = \case
  Or -> GOr
  And -> GAnd
  Plus -> GPlus
  Minus -> GMinus
  Modulo -> GModulo
  Mult -> GMult
  Divide -> GDivide
  Lt -> GLt
  Le -> GLe
  Gt -> GGt
  Ge -> GGe
  Eq -> GEq
  Ne -> GNe
  StrAppend -> GPlus

parseFunMetadata :: RuleMetadata -> GMetadata
parseFunMetadata metadata =
  case metadata.description of
    Just md -> GMkMetadata $ GString $ T.unpack md
    Nothing -> GNoMetadata

parseLit :: Lit -> GName
parseLit = \case
  IntLit int -> GMkName $ GString $ show int
  FracLit frac -> GMkName $ GString $ show frac
  BoolLit bool -> GMkName $ GString $ show bool
  StringLit string -> GMkName $ GString $ T.unpack string

parseExpr :: N.Name -> Expr -> GExpr
parseExpr name =
  let f = parseExpr name in \case
  Var var                  -> GVar (parseName var)
  Lit lit                  -> GVar (parseLit lit)
  Unary op expr            -> GUnary (parseUnaOp op) (f expr)
  BinExpr op l r           -> GBinExpr (parseBinOp op) (f l) (f r)
  IfThenElse cond thn els  -> GIfThenElse (f cond) (f thn) (f els)
  FunApp fun args          -> GFunApp (f fun) (GListExpr $ fmap f args)
--  Record rows              -> GRecord
  Project record label     -> GProject (f record) (parseName label)
  Fun md args body         -> GFun (parseName name) (parseFunMetadata md) (GListName $ fmap parseName args) (f body)
--  Let decl body            -> Let decl (f body)
  Predicate md args body   -> GPredicate (parseName name) (parseFunMetadata md) (GListName $ fmap parseName args) (f body)
  PredApp predicate args   -> GPredApp (f predicate) (GListExpr $ fmap f args)
--  Sig parents relations    -> Sig parents (traverse f) (tions
--)  StatementBlock statements  -> undefined -- TODO
  x -> error [i|parseExpr: not yet implemented #{x}|]



parseTypeDecl :: N.Name -> DataDecl -> GTypeDecl
parseTypeDecl name typedecl =
 GMkTypeDecl (parseMetadata typedecl) (parseName name) (parseRows typedecl) -- TODO: metadata
  where
    parseMetadata :: DataDecl -> GMetadata
    parseMetadata (RecordDecl _td _par metadata) =
      case metadata.description of
        Just md -> GMkMetadata $ GString $ T.unpack md
        Nothing -> GNoMetadata

parseRows :: DataDecl -> GListRowTypeDecl
parseRows = \case
  RecordDecl rowtypedecls _parents _metadata ->
    GListRowTypeDecl (parseRow <$> rowtypedecls)

parseRow :: RowTypeDecl -> GRowTypeDecl
parseRow rtd = GMkRowDecl GNoMetadata $ parseName rtd.name
  -- where
  --   metadata = (GMkMetadata . GString . T.unpack) <$> rtd.metadata.description


