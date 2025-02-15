{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns          #-}

{- |
  Sep 4 2024: The Langium-grammar constructs that aren't yet supported by the parser are mostly list exprs and the experimental normative ones.

  This module parses the JSON representation of (the concrete syntax of)
  an input Lam4 expression from Langium frontend

    ---------------------------
    Assumptions / preconditions
    ---------------------------
    This *input Lam4 expression*
    1. is valid, according to Langium parser
    2. is valid, according to scoper
    3. is valid, according to type checker
    4. is valid, according to the other static semantic checks / validators

In short: the expression conforms, not just to the Lam4 grammar, but also to its semantics.

The parsing/translation in Parser.hs preserves well-scopedness etc
  because the translation maps constructs in the Langium grammar to concrete syntax in a 1-to-1 way.
  The NamedElements are basically just relabelled with unique integers.

  Probably the most delicate part of the parsing is the handling of RecordLabels; see the @initializeEnvs@ function for discussion of that.

  ----------
    TODOs
  ----------
  * Improve error logging -- this is probably the place that needs that the most
  * Find a way to automatedly keep the Langium grammar in sync with this backend parser.
      * At the very least, write property based tests

  Style / code clarity:
  * Right now I'm using a mix of different idioms for interacting with JSON objects. I should standardize this when time permits
-}

module Lam4.Expr.Parser (
  parseProgramByteStr,
  parseProgram,
  parseExpr)
  where

import           Base                     hiding (throwError)
import           Base.Aeson               (FromJSON, _Integer, _Object, _String, _Bool,
                                           cosmos, values)
import qualified Base.Aeson               as A
import           Base.ByteString          (ByteString)
import qualified Base.Text                as T
import qualified Data.Foldable            as F
import qualified Data.Set                 as Set
import           Lam4.Expr.CommonSyntax   (RecordDeclMetadata (..),
                                           RowMetadata (..), RuleMetadata (..),
                                           Transparency (..), emptyRuleMetadata)
import           Lam4.Expr.ConcreteSyntax
import           Lam4.Expr.Name
import           Lam4.Parser.Monad
import           Lam4.Parser.Type

-- | The Langium-parser ASTNode `$type`s that correspond to recursive exprs
recursiveTypes :: Set Text
recursiveTypes = Set.fromList ["LetExpr", "FunDecl", "PredicateDecl", "AnonFunction", "VarDeclStmt"]
-- TODO: Make the categorization of recursive vs non-recursive decls more robust
-- TODO: Improve translation of LetExpr when it comes to LetRec vs Let (if necessary)


{----------------------
    Ref
-----------------------}

{-| A Ref differs from a WrappedRef in that it isn't wrapped in the "value" field.
TODO: May not need this if not using Relatums -}
newtype Ref = MkRef A.Object
  deriving newtype (Eq, Show, Ord, FromJSON)

{- | Example of a JSON object that corresponds to a 'WrappedRef':

      @
        {
          "$type": "Ref",
          "value": {
            "$ref": "#/elements@2/params@0",
            "$refText": "applicant"
          }
      @

-}
newtype WrappedRef = MkWrappedRef A.Object
  deriving newtype (Eq, Show, Ord, FromJSON)

parseRefToVar :: WrappedRef -> Parser Expr
parseRefToVar ref = Var <$> relabelRef ref

parseBareRefToVar :: Ref -> Parser Expr
parseBareRefToVar ref = Var <$> relabelBareRef ref

relabelBareRef :: Ref -> Parser Name
relabelBareRef = relabelRefHelper getRefPath getRefText
  where
    getRefPath node = node ^? ix "$ref" % _String
    getRefText node = node ^? ix "$refText" % _String

{- | Relabel an object that is a ('wrapped') `Ref` to a `Name`.

An example of a 'wrapped' Ref:
  @
  {
    "$type": "Ref",
    "value": {
        "$ref": "#/elements@2",
        "$refText": "some fact"
    }
  }
  @

A wrapped Ref differs from a bare/unwrapped one in that the underlying bare ref is tucked into the @value@ field.
-}
relabelRef :: WrappedRef -> Parser Name
relabelRef wrappedRef = relabelRefHelper getRef getRefText (coerce wrappedRef)
  where
    getRef node = node ^? ix "value" % ix "$ref" % _String
    getRefText node = node ^? ix "value" % ix "$refText" % _String

relabelRefHelper :: (A.Object -> Maybe RefPath) -> (A.Object -> Maybe Text) -> Ref -> Parser Name
relabelRefHelper getRefPath getRefText (MkRef node) = do
  -- @getRefPath@ here means: get the value of the @$ref@ field; this will be a json path
  let refPath = getRefPath node
      refText = getRefText node
  case (refPath, refText) of
    (Just refPath', Just refText') -> do
      makeNameHelper refText' refPath'
    _ -> throwError $ "[relabelRefHelper]\n" <> ppShow node <> " impossible"

{----------------------
    Program
-----------------------}

parseProgramByteStr :: ByteString -> Parser CSTProgram
parseProgramByteStr bs =
  case bs ^? _Object of
    Just programObject -> parseProgram programObject
    Nothing            -> pure []

parseProgram :: A.Object -> Parser CSTProgram
parseProgram program = do
  let
      elementValues = program ^.. ix "elements" % values
      elementObjects = elementValues ^.. folded % _Object

      objectsWithNodePaths = elementValues ^.. folded % cosmos % filtered (has (ix "nodePath"))
  _ <- initializeEnvs objectsWithNodePaths elementValues
  parseToplevelDecls elementObjects

initializeEnvs ::  [A.Value] -> [A.Value] -> Parser ()
initializeEnvs objectsWithNodePaths programElementValues = do
  let
    -- list of JSON paths for every NamedElement in the program
    nodePaths :: [RefPath] = objectsWithNodePaths ^.. folded % ix "nodePath" % _String

    -- isEntrypoint :: A.Value -> Bool
    isEntrypoint node = has (ix "isEntrypoint") node && (node ^?! ix "isEntrypoint" % _Bool)
    nodePathsOfEntrypointNodes :: Set RefPath = Set.fromList $ objectsWithNodePaths ^.. folded % filtered isEntrypoint % ix "nodePath" % _String
    nodeNameStatuses :: [ReferentStatus] = map (\nodePath ->
                                                  if nodePath `Set.member` nodePathsOfEntrypointNodes then IsEntrypoint
                                                  else NotEntrypoint)
                                            nodePaths

  {- Make Env of nodePaths (aka RefPaths) => Uniques
    All that's needed, for now, is *some* canonical order on the Uniques
  -}
  setEnv                $ zip nodePaths [1 .. ]
  setNodeNameStatusEnv  $ zip [1 ..] nodeNameStatuses

  {-
    ----------------------------------------------------------------
    TODO end Sep 2024: The recordLabelMap stuff was disabled for demo
    ----------------------------------------------------------------

    Then make a map of record labels => Names,
     to be used when introducing record exprs or destructuring them with record projections.
     This is what guarantees that the names used in projection will be in sync with those used when introducing the record exprs.

     TODO: This currently requires that the record label names be unique within a program, like Haskell,
     but it's not hard to lift this restriction --- e.g., by forwarding info from type checker / inference. Just not doing that right now because of time constraints.
  -}
  makeAndSetRecordLabelMap programElementValues
    where
      {- | Assumes you've __already__ made an Env of nodePaths => Uniques -}
      makeAndSetRecordLabelMap :: [A.Value] -> Parser ()
      makeAndSetRecordLabelMap progElementValues = do
        let allRowTypesFromRecDecls = progElementValues ^.. folded
                                    % filteredBy (ix "$type" % only "RecordDecl")
                                    % ix "rowTypes"
                                    % values
                                    % _Object

        labelNames <- traverse getName allRowTypesFromRecDecls
        let recordLabelAssocList = map (\name -> (name.name :: RecordLabel, name)) labelNames
        setRecordLabelEnv recordLabelAssocList


{-| Constructor for non-ReportDecl @Decl@s (TODO: Clean this up later).
Note: typeOfNode here refers to the @$type@; i.e., the type of an @AstNode@ from the Langium parser -}
mkNonEvalDecl :: Text -> Name -> Expr -> Decl
mkNonEvalDecl typeOfNode name expr
  | typeOfNode `elem` recursiveTypes = Rec name expr
  | otherwise = NonRec name expr

parseToplevelDecls :: [A.Object] -> Parser [Decl]
parseToplevelDecls = traverse parseToplevelDecl

parseToplevelDecl :: A.Object -> Parser Decl
parseToplevelDecl obj = do
  eltType <- obj .: "$type"
  case eltType of
    -- non-expr, non-statement decls
    "VarDeclStmt"   -> parseVarDecl obj
    "RecordDecl"    -> parseRecordDecl obj
    "ReportDecl"    -> Eval <$> parseExpr (getValueFieldOfNode obj)
    
    -- the postprocessing / normalization of the Entrypoint function/predicate's name will happen later, in a post-processing step
    -- expr decls (including FunDecl, PredicateDecl)
    _ ->  do
      expr <- parseExpr obj
      name <- getName obj
      pure $ mkNonEvalDecl eltType name expr

  
{----------------------
    parseExpr
-----------------------}

parseExpr :: A.Object -> Parser Expr
parseExpr node = do
  (node .: "$type" :: Parser Text) >>= \case
    "Ref"            -> parseRefToVar            (coerce node)

    -- literals
    "IntegerLiteral" -> parseIntegerLiteral node
    "DecimalLiteral" -> parseDecimalLiteral node
    "StringLiteral"  -> parseStringLiteral node
    "BooleanLiteral" -> parseBooleanLiteral node

    "LetExpr"        -> parseLet            node
    "FunDecl"        -> parseFunE           node
    "AnonFunction"   -> parseAnonFun node
    "PredicateDecl"  -> parsePredicateE     node

    "FunctionApplication"       -> parseFunApp node
    "InfixOrPostfixPredicateApplication" -> parseInfixPredicateApp node
    "PrefixPredicateApplication" -> parsePrefixPredicateApp node

    "BinExpr"        -> parseBinExpr        node

    "UnaryExpr"      -> parseUnaryExpr      node
    "IfThenElseExpr" -> parseIfThenElse     node


    "List"           -> parseList           node
    "Cons"           -> parseCons           node

    "Foldr"          -> parseFoldr          node
    "Foldl"          -> parseFoldl          node

    {-
    * Join: Join is currently disabled / deprecated, but may return if we want to do certain kinds of symbolic analysis
    * Sig: May want to remove Sigs as well.
            Not sure right now.
            For now, only allowing One Sig through the parser,
            and only One Sig with no Relations (which basically would be an Atom) for concrete-eval-only evaluators.

    -}
    "SigDecl"        -> parseSigE           node
    "RecordExpr"     -> parseRecordExprForDemo     node
    "Project"        -> parseProject        node

    typestr          -> throwError $ T.unpack typestr <> " not yet implemented"


{----------------------------------
    TypeExpr and Relation related
-----------------------------------}

parseBuiltinType :: Text -> Parser TyBuiltin
parseBuiltinType = \case
    "Integer" -> pure BuiltinTypeInteger
    "String"  -> pure BuiltinTypeString
    "Boolean" -> pure BuiltinTypeBoolean
    other     -> throwError ("Unexpected type " <> T.unpack other)

parseTypeExpr :: A.Object -> Parser TypeExpr
parseTypeExpr node = do
  (node .: "$type" :: Parser Text) >>= \case
    "CustomTypeDef" -> do
      name <- relabelBareRef $ coerce $ node `objAtKey` "annot"
      pure $ TyCustom name
    "BuiltinType"   -> do
      builtinType <- parseBuiltinType =<< (node .: "annot" :: Parser Text)
      pure $ TyBuiltin builtinType
    other           -> throwError $ "unrecognized type"  <> T.unpack other

parseRelation :: Name -> A.Object -> Parser Expr
parseRelation parentSigName relationNode = do
    relationName <- getName relationNode
    relatum      <- parseTypeExpr =<< relationNode .: "relatum"
    description  <- relationNode .:? "description"
    pure $ Relation relationName parentSigName relatum description


{----------------------
    Sig
-----------------------}

-- i.e., without worrying about the name -- that's handled by parseDecl
parseSigE ::  A.Object -> Parser Expr
parseSigE sigNode = do
  let
    parentRefNodes = getObjectsAtField sigNode "parents"
    relationNodes = getObjectsAtField sigNode "relations"
  unless (has (ix "multiplicity") sigNode) $ throwError "non-one-multiplicity Concepts / Sigs not currently supported by the evaluator"
  parents   <- traverse (relabelRef . coerce) parentRefNodes
  sigName   <- getName sigNode
  relations <- traverse (parseRelation sigName) relationNodes
  pure $ Sig parents relations


{-------------------------------------------
    BinExpr, Project, UnaryExpr, IfThenElse
---------------------------------------------}

parseBinExpr :: A.Object -> Parser Expr
parseBinExpr node = do
  op    <- parseBinOp =<< node .: "op"
  left  <- parseExpr  =<< node .: "left"
  right <- parseExpr  =<< node .: "right"
  pure $ BinExpr op left right

parseRecordExprForDemo :: A.Object -> Parser Expr
parseRecordExprForDemo node = do
  rows <- traverse mkBinding (node `getObjectsAtField` "rows")
  pure $ Record rows
  where
    mkBinding :: A.Object -> Parser (Name, Expr)
    mkBinding row = do
      labelName        <- row .: "label"
      let name = MkName labelName Nothing NotEntrypoint
      expr <- parseExpr =<< row .: "value"
      pure (name, expr)


{- | Key invariant for the parsing:
  Record expressions have to be constructed / parsed in such a way that:
  * the labels / names of the rows are __the same ones__ that a valid __projection__ of the record would use.

  So if I'm parsing @Project@ in such a way that the names used by the projection correspond to those in the original record declaration,
  my record expressions must similarly use names that refer
  to the labels / names in the original record declarations.
  This invariant is established by making a record label env with the record labels,
  and using those canonical names when creating record exprs and when projecting them.
-}
parseRecordExpr :: A.Object -> Parser Expr
parseRecordExpr node = do
  rows <- traverse mkBinding (node `getObjectsAtField` "rows")
  pure $ Record rows
  where
    mkBinding :: A.Object -> Parser (Name, Expr)
    mkBinding row = do
      name <- lookupRecordLabel =<< row .: "label"
      expr <- parseExpr =<< row .: "value"
      pure (name, expr)

{-| The Name used in the projection will correspond to that used when constructing the relevant record expr,
    because the Ref from the Langium parser will refer to
      the RowType in the relevant record declaration.

    __Examples__

    A RecordDecl would look like this:

    @
    {
      "$type": "RecordDecl",
      "name": "Applicant",
      "rowTypes": [
        {
          "$type": "RowType",
          "name": "publications",
          "value": {
            "$type": "BuiltinType",
            "annot": "Integer"
          },
          "nodePath": "#/elements@0/rowTypes@0"
        }
      ],
      "parents": [],
      "nodePath": "#/elements@0"
    },
    @

    And then a projection would look like this

    @
    {
        "$type": "Project",
        "left": {
          "$type": "Ref",
          "value": {
            "$ref": "#/elements@2/params@0",
            "$refText": "applicant"
          }
        },
        "right": {
          "$type": "Ref",
          "value": {
            "$ref": "#/elements@0/rowTypes@0",
            "$refText": "publications"
          }
        }
    @
-}
parseProject ::  A.Object -> Parser Expr
parseProject node = do
  left <- parseExpr =<< node .: "left"
  recordLabelName <- relabelRefForDemo $ coerce $ node `objAtKey` "right"
  pure $ Project left recordLabelName

relabelRefForDemo :: WrappedRef -> Parser Name
relabelRefForDemo node = pure $ MkName (fromJust refText) Nothing NotEntrypoint
  where
    coercedNode :: A.Object = coerce node
    refText = coercedNode ^? ix "value" % ix "$refText" % _String

parseBinOp :: A.Object -> Parser BinOp
parseBinOp opObj = do
  opStr <- opObj .: "$type"
  case opStr of
    "OpOr"        -> pure Or
    "OpAnd"       -> pure And
    "OpPlus"      -> pure Plus
    "OpMinus"     -> pure Minus
    "OpMult"      -> pure Mult
    "OpDiv"       -> pure Divide
    "OpLt"        -> pure Lt
    "OpLte"       -> pure Le
    "OpGt"        -> pure Gt
    "OpGte"       -> pure Ge
    "OpEquals"    -> pure Eq
    "OpNotEquals" -> pure Ne
    "OpModulo"    -> pure Modulo
    _             -> throwError $ "Unknown operator: " <> opStr

parseUnaryExpr :: A.Object -> Parser Expr
parseUnaryExpr node = do
    let op = node ^?! ix "op" ^?! ix "$type" % _String
    value <- parseExpr $ getValueFieldOfNode node
    case op of
        "OpMinus"             -> pure $ Unary UnaryMinus value
        "OpNot"               -> pure $ Unary Not value
        "OpFloor"             -> pure $ Unary Floor value
        "OpCeiling"           -> pure $ Unary Ceiling value
        "OpIntegerToFraction" -> pure $ Unary IntegerToFraction value
        _                     -> throwError $ "Unknown unary operator: " <> T.unpack op

parseIfThenElse :: A.Object -> Parser Expr
parseIfThenElse obj = do
    condition <- parseExpr =<< obj .: "condition"
    thenExpr  <- parseExpr =<< obj .: "then"
    elseExpr  <- parseExpr =<< obj .: "else"
    pure $ IfThenElse condition thenExpr elseExpr


{------------------------
    List, Cons
-------------------------}

parseList :: A.Object -> Parser Expr
parseList listNode = do
  elts <- traverse parseExpr (listNode `getObjectsAtField` "elements")
  pure $ List elts

parseCons :: A.Object -> Parser Expr
parseCons consNode = do
  first <- parseExpr =<< consNode .: "first"
  rest  <- parseExpr =<< consNode .: "rest"
  pure $ Cons first rest

{------------------------
    Builtin ListOps
-------------------------}

parseFoldr :: A.Object -> Parser Expr
parseFoldr foldrNode = do
  func <- parseExpr =<< foldrNode .: "combine"
  initVal <- parseExpr =<< foldrNode .: "nil"
  list <- parseExpr =<< foldrNode .: "collection"
  pure $ Foldr func initVal list

parseFoldl :: A.Object -> Parser Expr
parseFoldl foldlNode = do
  func <- parseExpr =<< foldlNode .: "update"
  initVal <- parseExpr =<< foldlNode .: "initial"
  list <- parseExpr =<< foldlNode .: "collection"
  pure $ Foldl func initVal list


{------------------------
    Let
-------------------------}

parseLet :: A.Object -> Parser Expr
parseLet obj = do
  rows <- traverse parseVarDecl $ obj `getObjectsAtField` "vars"
  body <- parseExpr $ obj `objAtKey` "body"
  makeNestedLet body rows
    where
      makeNestedLet :: Expr -> [Decl] -> Parser Expr
      makeNestedLet body rows = F.foldrM nestLet body rows

      nestLet :: Decl -> Expr -> Parser Expr
      nestLet binding accE = pure $ Let binding accE

parseVarDecl :: A.Object -> Parser Decl
parseVarDecl varDecl = do
  -- TODO: Check -- is this really how we want to handle name of varDecl?
  name <- getName varDecl
  let valObj = getValueFieldOfNode varDecl
  val <- parseExpr valObj
  valLangiumParserNodeType <- valObj .: "$type"
  pure $ mkNonEvalDecl valLangiumParserNodeType name val

parseRecordDecl :: A.Object -> Parser Decl
parseRecordDecl recordDecl = do
  recordName   <- getName recordDecl
  recordMetadata <- parseRecordMetadata recordDecl
  parents      <- traverse (relabelRef . coerce) (recordDecl `getObjectsAtField` "parents")
  rowTypeDecls <- traverse parseRowTypeDecl (recordDecl `getObjectsAtField` "rowTypes")
  pure $ mkRecordDecl recordName rowTypeDecls parents recordMetadata

parseRecordMetadata :: A.Object -> Parser RecordDeclMetadata
parseRecordMetadata recordDecl = do
  transparency <- tryParseTransparency recordDecl
  let description = getDescriptionFromNodeWithSpecificMetadataBlock recordDecl
  pure $ RecordDeclMetadata transparency description

parseRowTypeDecl :: A.Object -> Parser RowTypeDecl
parseRowTypeDecl rowTypeDecl =
  (rowTypeDecl .: "$type" :: Parser Text) >>= \case
    "RowType" -> do
      rowName <- getName rowTypeDecl
      typeAnnot <- parseTypeExpr =<< rowTypeDecl .: "value"
      description <- rowTypeDecl .:? "description"
      pure MkRowTypeDecl{name=rowName, typeAnnot=typeAnnot, metadata=MkRowMetadata description}
    other -> throwError $ "getting " <> T.unpack other <> " but shld be RowType"

{------------------------
    Function, Predicate
-------------------------}

parseOriginalRuleRef :: Maybe A.Object -> Parser (Maybe OriginalRuleRef)
parseOriginalRuleRef (Just origRuleRef) = do
  let ruleRef = origRuleRef ^?! ix "section" % _Integer % to show % to T.pack
  pure $ Just $ MkOriginalRuleRef ruleRef
parseOriginalRuleRef Nothing = pure Nothing

extractOriginalRuleRef :: A.Object -> Parser (Maybe OriginalRuleRef)
extractOriginalRuleRef node = parseOriginalRuleRef (node ^? ix "originalRuleRef" % _Object)

parseParam :: A.Object -> Parser Name
parseParam = getName

{- | Example of an AnonFunction node

  @REPORT (\a b => 1 + a + b)

  parses to

  @
  {
        "$type": "AnonFunction",
        "params": [
            {
                "$type": "Param",
                "name": "a",
                "nodePath": "#/elements@0/value/params@0"
            },
            {
                "$type": "Param",
                "name": "b",
                "nodePath": "#/elements@0/value/params@1"
            }
        ],
        "body": {
            "$type": "BinExpr",
            "left": {
                "$type": "BinExpr",
                "left": {
                    "$type": "IntegerLiteral",
                    "value": "1"
                },
                "op": {
                    "$type": "OpPlus"
                },
                "right": {
                    "$type": "Ref",
                    "value": {
                        "$ref": "#/elements@0/value/params@0",
                        "$refText": "a"
                    }
                }
            },
            "op": {
                "$type": "OpPlus"
            },
            "right": {
                "$type": "Ref",
                "value": {
                    "$ref": "#/elements@0/value/params@1",
                    "$refText": "b"
                }
            }
        }
    }
  @
-}
parseAnonFun :: A.Object -> Parser Expr
parseAnonFun anonFun = do
  paramNames   <- traverse parseParam (anonFun `getObjectsAtField` "params")
  body       <- parseExpr =<< anonFun .: "body"
  pure $ Fun emptyRuleMetadata paramNames body


parseFunE :: A.Object -> Parser Expr
parseFunE fun = do
  paramNames   <- traverse parseParam (fun `getObjectsAtField` "params")
  body         <- parseExpr =<< fun .: "body"
  ruleMetadata <- parseRuleMetadata fun
  pure $ Fun ruleMetadata paramNames body

{- | Treating predicate exprs separately from function expressions,
even though the current code is similar,
because predicates will likely be treated differently in symbolic execution
(and hence the specifications for functions versus predicates are likely to diverge)
-}
parsePredicateE :: A.Object -> Parser Expr
parsePredicateE predicate = do
  paramNames       <- traverse parseParam (getPredicateParams predicate)
  body             <- parseExpr =<< predicate .: "body"
  ruleMetadata     <- parseRuleMetadata predicate
  pure $ Predicate ruleMetadata paramNames body
    where
      getPredicateParams predicateNode = predicateNode ^.. ix "params" % values % ix "param" % _Object

-- | the arg to a InfixOrPostfixPredicateApplication can be either a NamedElement:ID or Expr
parsePredicateAppArg :: A.Object -> Parser Expr
parsePredicateAppArg arg =
  if has (ix "$type") arg
  then parseExpr arg
  else parseBareRefToVar (coerce arg)

{- | Sep 18 2024: I'm desugaring this to PredApp, instead of adding a separate InfixPredApp construct,
because having a more faithful concrete syntax is not the priority right now.
But will add that when time permits, since it is useful for things like rendering, synchronization, automated refactoring.
-}
parseInfixPredicateApp ::  A.Object -> Parser Expr
parseInfixPredicateApp predApp = do
    predicate <- parseBareRefToVar =<< predApp .: "predicate"
    left  <- parseExpr  =<< predApp .: "left"
    -- the @right@ may not be present
    right <- traverse parseExpr $ predApp ^? ix "right" % _Object
    pure $ case right of
      Just rightExpr ->
        let args = [left, rightExpr]
        in PredApp predicate args
      Nothing ->
        PredApp predicate [left]

{- | Example of a PrefixPredicateApplication node (Sep 18 2024):

    @
    {
        "$type": "PrefixPredicateApplication",
        "predicate": {
            "$type": "Ref",
            "value": {
                "$ref": "#/elements@2",
                "$refText": "some fact"
            }
        }
    }
    @
-}
parsePrefixPredicateApp ::  A.Object -> Parser Expr
parsePrefixPredicateApp predApp = do
    predicate <- parseRefToVar =<< predApp .: "predicate"
    args      <- traverse parsePredicateAppArg (predApp `getObjectsAtField` "args")
    pure $ PredApp predicate args


parseFunApp :: A.Object -> Parser Expr
parseFunApp funApp = do
    func <- parseExpr =<< funApp .: "func"
    args <- traverse parseExpr (funApp `getObjectsAtField` "args")
    pure $ FunApp func args


{----------------------
    Literals
-----------------------}

parseStringLiteral :: A.Object -> Parser Expr
parseStringLiteral literalNode = do
    let literalVal = literalNode ^? ix "value" % _String
    case literalVal of
      Just litVal -> pure . Lit $ StringLit litVal
      Nothing -> throwError $ "Failed to parse string value. Node: " <> ppShow literalNode

parseIntegerLiteral :: A.Object -> Parser Expr
parseIntegerLiteral literalNode = do
    let literalVal = literalNode ^? ix "value" % _String % _Integer % to fromInteger
    case literalVal of
      Just litVal -> pure . Lit $ IntLit litVal
      Nothing -> throwError $ "Failed to parse integer value. Node: " <> ppShow literalNode

{- | Example of DecimalLiteral node:

@
  {
    "$type": "DecimalLiteral",
    "value": "1.333"
  }
@
-}
parseDecimalLiteral :: A.Object -> Parser Expr
parseDecimalLiteral literalNode = do
    let literalVal = literalNode ^? ix "value" % _String
    case literalVal of
      Just litVal ->
        case T.rational litVal of
          Right (litVal', _) -> pure . Lit $ FracLit litVal'
          Left err -> throwError $ "Failed to parse decimal literal. " <> err <>  "Node: " <> ppShow literalNode
      Nothing -> throwError $ "Failed to parse decimal literal. Node: " <> ppShow literalNode

parseBooleanLiteral :: A.Object -> Parser Expr
parseBooleanLiteral literalNode = do
    let literalVal = literalNode ^? ix "value" % _String
    case literalVal of
      Just "True" -> pure . Lit $ BoolLit True
      Just "False" -> pure . Lit $ BoolLit False
      _ -> throwError $ "Failed to parse boolean literal. Node: " <> ppShow literalNode


{----------------------
    Utils
-----------------------}

{- | The input must be a node with the @WithSpecificMetadataBlock@ fragment in the Langium grammar.
-}
getDescriptionFromNodeWithSpecificMetadataBlock :: A.Object -> Maybe Text
getDescriptionFromNodeWithSpecificMetadataBlock node  = node ^? ix "metadata" % ix "description" % ix "value" % _String

tryParseTransparency :: A.Object -> Parser Transparency
tryParseTransparency (getTransparency -> Just "Opaque")      = pure Opaque
tryParseTransparency (getTransparency -> Just "Transparent") = pure Transparent
tryParseTransparency _                                       = pure Transparent

getTransparency :: A.Object -> Maybe Text
getTransparency node = node ^? ix "transparency" % ix "$type" % _String

parseRuleMetadata :: A.Object -> Parser RuleMetadata
parseRuleMetadata node = do
  description  <- node .:? "description"
  originalRuleRef  <- extractOriginalRuleRef node
  transparency     <- tryParseTransparency node
  pure $ RuleMetadata{originalRuleRef, transparency, description}


{- | Gets/Makes the Name of an NamedElement node
      that has both a `name` and `nodePath` key.
    Assumes that the node's refPath has already been stashed in the environment
-}
getName :: A.Object -> Parser Name
getName node = do
  name :: Text <- node .: "name"
  nodePath :: Text <- node .: "nodePath"
  makeNameHelper name nodePath

makeNameHelper :: Text -> RefPath -> Parser Name
makeNameHelper nameText nodePath = do
  unique                 <- refPathToUnique nodePath
  referentStatus <- lookupReferentNodeStatus unique
  pure $ MkName nameText (Just unique) referentStatus


getValueFieldOfNode :: (JoinKinds (IxKind s) A_Prism k, Is k An_AffineFold, Ixed s,  A.AsValue (IxValue s), IsString (Index s)) => s -> A.Object
getValueFieldOfNode node = node `objAtKey` "value"
