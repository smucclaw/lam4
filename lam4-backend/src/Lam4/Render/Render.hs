{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}

module Lam4.Render.Render (NLGConfig (..), NLGEnv, makeNLGEnv, renderCstProgramToNL) where

import qualified Base.Text                as T
import           Control.Lens             ((%~), (&))
import           Control.Lens.Regex.Text  (match, regex)
import           Data.String.Interpolate  (i)
import           Text.RawString.QQ
import           Lam4.Expr.CommonSyntax
import           Lam4.Expr.ConcreteSyntax
import qualified Lam4.Expr.Name           as N (Name (..), ReferentStatus (..))
import           Paths_lam4_backend       (getDataFileName)

-- GF-related stuff
import           Lam4.Render.Lam4Gf
import qualified PGF

-- TODO: Make a ToNLG monad with NLGEnv as the Reader arg

type GFLinearizer = PGF.Tree -> T.Text

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
  { gfLin     :: GFLinearizer
  , gfTree    :: GFLinearizer
  }

gfPath :: String -> String
gfPath x = [i|gf-grammar/#{x}|]

-- | Smart constructor that initializes the GFLinearizer
makeNLGEnv :: NLGConfig -> IO NLGEnv
makeNLGEnv config = do
  -- TODO: In the future, the GF-specific paths will be loaded from cmd line args, though we could have 'default' filenames or smtg

  -- Load grammar file
  grammarFile <- getDataFileName $ gfPath config.abstractSyntaxFilename
  gr <- PGF.readPGF grammarFile

  -- Set up PGF Language and GF Linearizer
  let lang       = initializeGFLang config.concreteSyntaxName gr
      linearizer = makeGFLinearizer gr lang
      printTree  = T.pack . PGF.showExpr []
  pure $ NLGEnv linearizer printTree

makeGFLinearizer :: PGF.PGF -> PGF.Language -> GFLinearizer
makeGFLinearizer gr lang = postprocessText . T.pack . PGF.linearize gr lang

initializeGFLang :: String -> PGF.PGF -> PGF.Language
initializeGFLang str gr =
  case (PGF.readLanguage str, PGF.languages gr) of
    (Just l, langs)  -- Language looks valid, check if in grammar
      -> if l `elem` langs
            then l
            else error [i|Render.getLang: #{str} not found among #{langs}|]
    (Nothing, langs)
      -> error [i|Render.getLang: #{str} not a valid language. (GF grammar contains #{langs}.)|]


postprocessText :: T.Text -> T.Text
postprocessText = rmBIND . newlines -- . any other postprocessing functions here
  where
    -- TODO: the following could be cleaned up / made clearer
    rmBIND :: T.Text -> T.Text
    rmBIND input = input & [regex|\s+&\+\s+|] . match %~ const ""

    newlines :: T.Text -> T.Text
    newlines = T.map (\c -> if c == '∞' then '\n' else c)

style :: T.Text
style = [r|
    <style>
        /* Insert the CSS styling here */

        /* Base styles for dl, dt, dd */
        dl {
          margin: 0; /* Remove default margin from dl */
          padding: 0; /* Remove default padding from dl */

        }

        dt, dd {
          margin: 0;
          padding: 0.25em 0;
          padding-left: 0.5em;
        }

        dt + dd {
          margin-top: 0.01em; /* Small margin between dt and dd */
        }

        /* Level 2 styles */
        dl dl {
          background-color: #f4f6f6; /* gray */
          padding-left: 0.5em;
          border-color: #00796b;
        }

        dl dl > dt {
          font-weight: bold;
          color: #004d40;
        }

        dl dl > dd {
          margin-left: 1em;
        }

        /* Level 3 styles */
        dl dl dl {
          background-color: #fff1ed; /* Light pink */
          padding-left: 0.5em;
        }

        dl dl dl > dt {
          font-weight: bold;

          color: #880e4f;
        }

        dl dl dl > dd {
          margin-left: 1em;
        }

        /* Level 4 styles */
        dl dl dl dl {
          background-color: #ffe0b2; /* Light orange */
          padding-left: 0.5em;
          border-color: #f57c00;

        }

        dl dl dl dl > dt {
          font-weight: bold;
          color: #e65100;
        }

        dl dl dl dl > dd {
          margin-left: 1em;
        }


        /* Level 5 styles */
        dl dl dl dl dl {
          background-color: #e9ffd2; /* Light green */
          padding-left: 0.5em;
        }

        dl dl dl dl dl > dt {
          font-weight: bold;
          color: #79b47e;

        }

        dl dl dl dl dl > dd {
          margin-left: 1em;
        }
        /* Add more levels as needed */

      </style>|]

-- | Entrypoint
renderCstProgramToNL :: NLGEnv -> CSTProgram -> T.Text
renderCstProgramToNL env decls = T.unlines (
  ["<html>", "<head>", style, "</head>", "<body>"] <>
  fmap (renderCstDeclToNL env) decls  <>
  ["</body>", "</html>"]
--  <> fmap (renderCstDeclToGFtrees env) decls
  )

renderCstDeclToNL :: NLGEnv -> Decl -> T.Text
renderCstDeclToNL env = gfLin env . gf . genericTreeTrans . parseDecl

renderCstDeclToGFtrees :: NLGEnv -> Decl -> T.Text
renderCstDeclToGFtrees env = gfTree env . gf . genericTreeTrans . parseDecl


-- TODO: do we flatten nested Let-definitions?
-- for royalflush case, that'd be the best thing to do
-- how about generally?
parseDecl :: Decl -> GS
parseDecl = \case
  DataDecl name typedecl -> GTypeDeclS dummyId $ parseTypeDecl name typedecl
  Rec name expr ->
    if commonFunction name.name
      then GExprS dummyId $ GKnownFunction $ parseName name
      else GExprS dummyId $ parseExpr name expr
  NonRec name (Sig [] []) -> GAtomicConcept dummyId (parseName name)
  NonRec name expr@(BinExpr binop _ _) ->
    if booleanOp binop
      then GLetIsTrue dummyId (parseName name) $ parseExpr noName expr
      else GAssignS dummyId (parseName name) $ parseExpr noName expr
  NonRec name expr -> GAssignS dummyId (parseName name) $ parseExpr noName expr
  Eval expr -> quoteVars $ if isBool expr
                then GEvalWhetherS dummyId $ parseExpr noName expr
                else GEvalS dummyId $ parseExpr noName expr

-- to wrap all declarations in <p id="paragraph_999999"> … </p>
dummyId :: GString
dummyId = GString "paragraph_999999"

noName :: N.Name
noName = N.MkName mempty Nothing N.NotEntrypoint

parseName :: N.Name -> GName
parseName = GMkName . GString . T.unpack . N.name

parseNameForRecord :: N.Name -> GName
parseNameForRecord = GMkName . GString . T.unpack . rmThe . N.name
  where
    rmThe :: T.Text -> T.Text
    rmThe input = input & [regex|^\s?the+\s+|] . match %~ const ""

commonFunction :: T.Text -> Bool
commonFunction x = T.unpack x `elem` ["id", "map", "filter", "cons", "nil", "minus", "plus", "div", "mult", "add", "modulo", "pow", "round", "certain", "uncertain", "known", "unknown", "default", "instanceSumIf", "instanceSum"]

booleanOp :: BinOp -> Bool
booleanOp op = op `elem` [Eq, Lt, Gt, Le, Ge, Ne, And, Or]

isPredicate :: Expr -> Bool
isPredicate = \case
  Var (N.MkName name _ _) -> name `elem` ["certain", "known", "uncertain", "unknown"]
  _ -> False

varFromFun :: Expr -> Expr
varFromFun = \case
  Fun _md _args (Project _rec label) -> Var label
  e -> e
---- Tree transformations -----

genericTreeTrans :: Tree a -> Tree a
genericTreeTrans = flattenITE . flattenNestedAndOr . aggregatePredApp . binExprVerbosity

quoteVars :: Tree a -> Tree a
quoteVars (GVar x) = GQuoteVar x
quoteVars x        = composOp quoteVars x

-- Control verbosity of BinExpr in specific contexts
binExprVerbosity :: Tree a -> Tree a
binExprVerbosity (GAssignS id_ e (GBinExpr op lc rc)) = GAssignS id_ e (GVerboseBinExpr op (unVerboseBinExpr lc) (unVerboseBinExpr rc))
binExprVerbosity (GLetIsTrue id_ e (GBinExpr op lc rc)) = GLetIsTrue id_ e (GVerboseBinExpr op (unVerboseBinExpr lc) (unVerboseBinExpr rc))
binExprVerbosity (GVerboseBinExpr op lc rc) = GVerboseBinExpr op (unVerboseBinExpr lc) (unVerboseBinExpr rc)
binExprVerbosity x = composOp binExprVerbosity x

-- helper function for binExprVerbosity
unVerboseBinExpr :: Tree a -> Tree a
unVerboseBinExpr (GVerboseBinExpr op lc rc) = GBinExpr op lc rc
unVerboseBinExpr x = composOp unVerboseBinExpr x

aggregatePredApp :: Tree a -> Tree a
aggregatePredApp tree@(GBinExpr op (GPredApp f arg) (GPredApp g arg')) =
  if sameTree arg arg'
    then GPredAppMany op (GListExpr [f,g]) arg
    else tree
aggregatePredApp tree@(GVerboseBinExpr op (GPredApp f arg) (GPredApp g arg')) =
      if sameTree arg arg'
        then GPredAppMany op (GListExpr [f,g]) arg
        else tree
aggregatePredApp x = composOp aggregatePredApp x

flattenITE :: Tree a -> Tree a
flattenITE expr@GIfThenElse{} =
  case collectITE expr of
    ites@(_:_:_) -> GElif $ GListIfThen (rmElseFromFirst ites)
    _            -> expr
  where
    rmElseFromFirst (GMiddleIfThen i t : rest) = GFirstIfThen i t : rest
    rmElseFromFirst x = x
flattenITE x = composOp flattenITE x

collectITE :: GExpr -> [GIfThen]
collectITE (GIfThenElse i t e@GIfThenElse{}) = GMiddleIfThen i t : collectITE e
collectITE (GIfThenElse i t e) = [GMiddleIfThen i t, GNilIfThen e]
collectITE e = [GNilIfThen e]

flattenNestedAndOr :: Tree a -> Tree a
flattenNestedAndOr e@GBinExpr{} = composOp flattenNestedAndOr (flattenIfLongEnough e)
flattenNestedAndOr e@GVerboseBinExpr{} = composOp flattenNestedAndOr (flattenIfLongEnough e)
flattenNestedAndOr x = composOp flattenNestedAndOr x

flattenIfLongEnough :: GExpr -> GExpr
flattenIfLongEnough e =
  case orExprs of
    (_:_:_) -> GApplyListOp GListOr (GListLExpr orExprs)
    _ -> case andExprs of
           (_:_:_) -> GApplyListOp GListAnd (GListLExpr andExprs)
           _ -> e
  where
    orExprs = GcoerceListExpr <$> collectOr e
    andExprs = GcoerceListExpr <$> collectAnd e

    collectOr :: GExpr -> [GExpr]
    collectOr = \case
      GBinExpr        GOr left right -> collectOr left <> collectOr right
      GVerboseBinExpr GOr left right -> collectOr left <> collectOr right
      expr                    -> [expr]

    collectAnd :: GExpr -> [GExpr]
    collectAnd = \case
      GBinExpr        GAnd left right -> collectAnd left <> collectAnd right
      GVerboseBinExpr GAnd left right -> collectAnd left <> collectAnd right
      expr                           -> [expr]

--------------------------------

sameTree :: forall a. Gf (Tree a) => Tree a -> Tree a -> Bool
sameTree a b = show a == show b

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
  Unary UnaryMinus expr    -> GUnaryMinusExpr (f expr) ;
  Unary op expr            -> GUnary (parseUnaOp op) (f expr)
  -- e.g. "x / y"
  BinExpr op lc@Var{} rc@Var{}    -> GBinExpr (parseBinOp op) (f lc) (f rc)

  -- e.g. "x / b's y"
  BinExpr op lc@Var{} rc@Project{}    -> GBinExpr (parseBinOp op) (f lc) (f rc)

  -- e.g. "a's x / y"
  BinExpr op lc@Project{} rc@Var{} -> GBinExpr (parseBinOp op) (f lc) (f rc)

  -- e.g. "a's x / b's y"
  BinExpr op lc@Project{} rc@Project{} -> GBinExpr (parseBinOp op) (f lc) (f rc)

  -- other BinExprs are "verbose" = newlines and stuff
  BinExpr op lc rc         -> GVerboseBinExpr (parseBinOp op) (f lc) (f rc)
  IfThenElse cond thn els  -> GIfThenElse (f cond) (f thn) (f els)

  -- Basic arithmetic operations that have been defined as custom function
  FunApp (Var (N.MkName "div" _ _)) [lc,rc] -> parseExpr name (BinExpr Divide lc rc)
  FunApp (Var (N.MkName "mult" _ _)) [lc,rc] -> parseExpr name (BinExpr Mult lc rc)
  FunApp (Var (N.MkName "add" _ _)) [lc,rc] -> parseExpr name (BinExpr Plus lc rc)

  -- Basic arithmetic operation that is hardly domain-specific
  -- Current linearization of Round takes Expr, Int
  -- and just outputs the Expr instead of "Expr rounded into precision of Int decimals"
  -- But this should be configurable.
  FunApp (Var (N.MkName "round" _ _))       args -> parseRound args

  -- These should be replaced with a general annotation-based approach
  FunApp (Var (N.MkName "default" _ _))     args -> parseDefault args
  FunApp (Var (N.MkName "instanceSumIf" _ _)) args -> parseInstanceSum args
  FunApp (Var (N.MkName "instanceSum" _ _)) args -> parseInstanceSum args


  FunApp fun args -> if isPredicate fun
                    then parseExpr name (PredApp fun args)
                    else GFunApp (f fun) (GListExpr $ fmap f args)
--  FunApp fun args          -> GFunApp (f fun) (GListExpr $ fmap f args)
--  Record rows              -> GRecord
  Project record label     -> GOnlyFieldProject (f record) (parseNameForRecord label) -- TODO: annotation to decide whether to print out the record name or only label?
  Fun md args body         -> GFun (parseName name) (parseFunMetadata md) (GListName $ fmap parseName args) (f body)
  Predicate md args body   -> GPredicate (parseName name) (parseFunMetadata md) (GListName $ fmap parseName args) (f body)
  PredApp predicate args   -> GPredApp (f predicate) (GListExpr $ fmap f args)
  Foldr combine nil over   -> GFold (f combine) (f nil) (f over)
  Foldl combine nil over   -> GFold (f combine) (f nil) (f over)
  Sig parents relations    -> GSig (GListName $ fmap parseName parents) (GListExpr $ fmap f relations)
  Let decl expr            -> GLet (parseDecl decl) (f expr)
  Cons e1 e2               -> GConjExpr (GListExpr [f e1, f e2])
  List es                  -> GConjExpr (GListExpr $ fmap f es)
  Record rows              -> GConjExpr (GListExpr $ fmap parseRecordRow rows)
--)  StatementBlock statements  -> undefined -- TODO
  x -> error [i|parseExpr: not yet implemented #{x}|]

parseInstanceSum :: [Expr] -> GExpr
parseInstanceSum [_set, inst, cond] = GInstanceSumIf instExpr condExpr
  where
    instExpr = parseExpr noName $ varFromFun inst
    condExpr = parseExpr noName $ varFromFun cond
parseInstanceSum [_set, inst] = GInstanceSum instExpr
  where
    instExpr = parseExpr noName $ varFromFun inst
parseInstanceSum _ = GKnownFunction $ GMkName $ GString "SOMETHING WENT WRONG D:"

parseRound :: [Expr] -> GExpr
parseRound [expr, prec] = GRound (parseExpr noName expr) (parseExpr noName prec)
parseRound _ = GKnownFunction $ GMkName $ GString "SOMETHING WENT WRONG D:"

parseDefault :: [Expr] -> GExpr
parseDefault [expr, deflt] = GDefault (parseExpr noName expr) (parseExpr noName deflt)
parseDefault _ = GKnownFunction $ GMkName $ GString "SOMETHING WENT WRONG D:"

parseRecordRow :: (N.Name, Expr) -> GExpr
parseRecordRow (name, expr) = GRecord (parseName name) (parseExpr name expr)

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


