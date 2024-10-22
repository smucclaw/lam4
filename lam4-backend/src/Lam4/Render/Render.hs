{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}

module Lam4.Render.Render (NLGConfig (..), NLGEnv, makeNLGEnv, renderCstProgramToNL) where

import qualified Base.Text                as T
import           Control.Lens             ((%~), (&))
import           Control.Lens.Regex.Text  (match, regex)
import           Data.String.Interpolate  (i)
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax
import qualified Lam4.Expr.Name           as N (Name (..), ReferentStatus (..))
import           Paths_lam4_backend       (getDataFileName)

-- GF-related stuff
import           Lam4.Render.Lam4Gf
import qualified PGF

-- | Config that stores info about paths and various other NLG configuration things
data NLGConfig = MkNLGConfig {
      outputDir              :: FilePath
    , abstractSyntaxFilename :: FilePath
    -- ^ e.g. "Lam4.pgf"
    , concreteSyntaxName     :: String
    -- ^ e.g. "Lam4Eng"
}

-- Loosely copied from dsl/…/natural4
-- | Env that's needed for NLG operations
data NLGEnv = NLGEnv
  { gfGrammar :: PGF.PGF
  , gfLang    :: PGF.Language
--  , gfParse :: Type -> T.Text -> [Expr]
  , gfLin     :: PGF.Expr -> T.Text
  }

gfPath :: String -> String
gfPath x = [i|gf-grammar/#{x}|]

-- | Smart constructor that initializes the NLGEnv
makeNLGEnv :: NLGConfig -> IO NLGEnv
makeNLGEnv config = do
  -- TODO: In the future, the GF-specific paths will be loaded from cmd line args, though we could have 'default' filenames or smtg

  -- Load grammar file
  grammarFile <- getDataFileName $ gfPath config.abstractSyntaxFilename
  gr <- PGF.readPGF grammarFile

  -- Set up PGF Language and GF Linearizer
  let lang = initializeGFLang config.concreteSyntaxName gr
      linearizer = makeGFLinearizer gr lang
  pure $ NLGEnv gr lang linearizer

initializeGFLang :: String -> PGF.PGF -> PGF.Language
initializeGFLang str gr =
  case (PGF.readLanguage str, PGF.languages gr) of
    (Just l, langs)  -- Language looks valid, check if in grammar
      -> if l `elem` langs
            then l
            else error [i|Render.getLang: #{str} not found among #{langs}|]
    (Nothing, langs)
      -> error [i|Render.getLang: #{str} not a valid language. (GF grammar contains #{langs}.)|]

makeGFLinearizer :: PGF.PGF -> PGF.Language -> PGF.Tree -> T.Text
makeGFLinearizer gr lang = postprocessText . T.pack . PGF.linearize gr lang
  where
    postprocessText :: T.Text -> T.Text
    postprocessText = newlines . tabs . rmBIND

    -- TODO: the following could be cleaned up / made clearer
    rmBIND :: T.Text -> T.Text
    rmBIND input = input & [regex|\s+&\+\s+|] . match %~ const ""

    tabs :: T.Text -> T.Text
    tabs = T.map (\c -> if c == '°' then ' ' else c)

    newlines :: T.Text -> T.Text
    newlines = T.map (\c -> if c == '∞' then '\n' else c)

-- | Entrypoint
renderCstProgramToNL :: NLGEnv -> CSTProgram -> T.Text
renderCstProgramToNL env =  T.unlines . fmap  (renderCstDeclToNL env)

renderCstDeclToNL :: NLGEnv -> Decl -> T.Text
renderCstDeclToNL env = gfLin env . gf . parseDecl

parseDecl :: Decl -> GS
parseDecl = \case
  DataDecl name typedecl -> GTypeDeclS $ parseTypeDecl name typedecl
  Rec name expr -> GExprS $ parseExpr name expr
  NonRec name expr -> GAssignS (parseName name) $ parseExpr noName expr
  Eval expr -> quoteVars $ if isBool expr
                then GEvalWhetherS $ parseExpr noName expr
                else GEvalS $ parseExpr noName expr

noName :: N.Name
noName = N.MkName mempty Nothing N.NotEntrypoint

parseName :: N.Name -> GName
parseName = GMkName . GString . T.unpack . N.name

quoteVars :: Tree a -> Tree a
quoteVars (GVar x) = GQuoteVar x
quoteVars x        = composOp quoteVars x

isBool :: Expr -> Bool
isBool = \case
  BinExpr Eq _ _ -> True
  BinExpr Lt _ _ -> True
  BinExpr Le _ _ -> True
  BinExpr Gt _ _ -> True
  BinExpr Ge _ _ -> True
  BinExpr Ne _ _ -> True
  _ -> False

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
  Lit lit                  -> GLit (parseLit lit)
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
  Foldr combine nil over   -> GFold (f combine) (f nil) (f over)
  Foldl combine nil over   -> GFold (f combine) (f nil) (f over)
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
parseRow rtd = GMkRowDecl (parseMetadata rtd.metadata) (parseName rtd.name)
  where
    parseMetadata :: RowMetadata -> GMetadata
    parseMetadata mdata =
      case mdata.description of
        Just md -> GMkMetadata $ GString $ T.unpack md
        Nothing -> GNoMetadata


