{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE  ViewPatterns #-}

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
import           Base.Aeson               (FromJSON, _Integer, _Object, _String,
                                           cosmos, values)
import qualified Base.Aeson               as A
import           Base.ByteString          (ByteString)
import qualified Base.Text                as T
import qualified Data.Foldable            as F
import qualified Data.Set                 as Set
import           Lam4.Expr.CommonSyntax   (DeclF (..), RecordDeclMetadata (..),
                                           RuleMetadata (..), Transparency (..),
                                           emptyRuleMetadata)
import           Lam4.Expr.ConcreteSyntax
import           Lam4.Expr.Name           (Name (..))
import           Lam4.Parser.Monad
import           Lam4.Parser.Type


-- | The Langium-parser ASTNode `$type`s that correspond to recursive exprs
recursiveTypes :: Set Text
recursiveTypes = Set.fromList ["LetExpr", "FunDecl", "PredicateDecl"]

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

{- | Relabel an object that is a ('wrapped') `Ref` to a `Name` -}
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
      refUnique <- refPathToUnique refPath'
      pure $ MkName refText' refUnique
    _ -> throwError $ ppShow node <> " impossible"

{----------------------
    Program
-----------------------}

parseProgramByteStr :: ByteString -> Parser [Decl]
parseProgramByteStr bs =
  case bs ^? _Object of
    Just programObject -> parseProgram programObject
    Nothing            -> pure []

parseProgram :: A.Object -> Parser [Decl]
parseProgram program = do
  let
      elementValues = program ^.. ix "elements" % values
      elementObjects = elementValues ^.. folded % _Object
      -- list of JSON paths for every NamedElement in the program
      nodePaths = elementValues ^.. folded % cosmos % ix "nodePath" % _String

  initializeEnvs nodePaths elementValues
  parseDecls elementObjects

initializeEnvs :: [RefPath] -> [A.Value] -> Parser ()
initializeEnvs nodePaths programElementValues = do
  {- Make Env of nodePaths (aka RefPaths) => Uniques
    All that's needed, for now, is *some* canonical order on the Uniques
  -}
  setEnv (zip nodePaths [1 .. ])

  {- Then make a map of record labels => Names,
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


{-| Constructor for @Decl@s.
Note: typeOfNode here refers to the @$type@; i.e., the type of an @AstNode@ from the Langium parser -}
mkDecl :: Text -> Name -> Expr -> Decl
mkDecl typeOfNode name expr =
  if has (contains typeOfNode) recursiveTypes
  then Rec name expr
  else NonRec name expr

parseDecls :: [A.Object] -> Parser [Decl]
parseDecls = traverse parseDecl

parseDecl :: A.Object -> Parser Decl
parseDecl obj = do
  eltType <- obj .: "$type"
  case eltType of
    -- non-expr, non-statement decls
    "VarDeclStmt" -> parseVarDecl obj
    "RecordDecl"  -> parseRecordDecl obj

    -- exprs
    _ ->  do
      expr <- parseExpr obj
      name <- getName obj
      pure $ mkDecl eltType name expr

{----------------------
    parseExpr
-----------------------}

parseExpr :: A.Object -> Parser Expr
parseExpr node = do
  (node .: "$type" :: Parser Text) >>= \case
    "Ref"            -> parseRefToVar            (coerce node)


    -- literals
    "IntegerLiteral" -> parseIntegerLiteral node
    -- "StringLiteral"  -> parseLiteral StringLit node
    "BooleanLiteral" -> parseLiteral BoolLit node

    "LetExpr"        -> parseLet            node
    "FunDecl"        -> parseFunE           node
    "AnonFunction"   -> parseAnonFun node
    "PredicateDecl"  -> parsePredicateE     node

    "FunctionApplication"       -> parseFunApp node
    "InfixPredicateApplication" -> parsePredicateApp node

    "BinExpr"        -> parseBinExpr        node

    "UnaryExpr"      -> parseUnaryExpr      node
    "IfThenElseExpr" -> parseIfThenElse     node

    {-  Join is currently disabled / deprecated, but may return if we want to do certain kinds of symbolic analysis
      May want to remove Sigs as well. Not sure right now, and keeping it around for the time being makes the syntax for certain things a bit nicer
    -}
    "SigDecl"        -> parseSigE           node
    "RecordExpr"     -> parseRecordExpr     node
    "Project"        -> parseProject        node

    typestr          -> throwError $ T.unpack typestr <> " not yet implemented"


{----------------------------------
    TypeExpr and Relation related
-----------------------------------}

parseBuiltinType :: Text -> Parser BuiltinType
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
      pure $ CustomType name
    "BuiltinType"   -> do
      builtinType <- parseBuiltinType =<< (node .: "annot" :: Parser Text)
      pure $ BuiltinType builtinType
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
  recordLabelName <- relabelRef $ coerce $ node `objAtKey` "right"
  pure $ Project left recordLabelName

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
    op <- node .: "op"
    value <- parseExpr $ getValueFieldOfNode node
    case op of
        "OpMinus" -> pure $ Unary UnaryMinus value
        "OpNot"   -> pure $ Unary Not value
        _         -> throwError $ "Unknown unary operator: " <> op

parseIfThenElse :: A.Object -> Parser Expr
parseIfThenElse obj = do
    condition <- parseExpr =<< obj .: "condition"
    thenExpr  <- parseExpr =<< obj .: "then"
    elseExpr  <- parseExpr =<< obj .: "else"
    pure $ IfThenElse condition thenExpr elseExpr


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
  pure $ mkDecl valLangiumParserNodeType name val

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
  let description  = recordDecl ^? ix "description" % _String
  pure $ RecordDeclMetadata transparency description

parseRowTypeDecl :: A.Object -> Parser RowTypeDecl
parseRowTypeDecl rowTypeDecl =
  (rowTypeDecl .: "$type" :: Parser Text) >>= \case
    "RowType" -> do
      rowName <- getName rowTypeDecl
      typeExpr <- parseTypeExpr =<< rowTypeDecl .: "value"
      pure (rowName, typeExpr)
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

parseAnonFun :: A.Object -> Parser Expr
parseAnonFun anonFun = do
  paramNames <- traverse parseParam anonFunParams
  body       <- parseExpr =<< anonFun .: "body"
  pure $ Fun emptyRuleMetadata paramNames body
    where
      anonFunParams = anonFun ^.. ix "params" % values % cosmos % ix "param" % _Object

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

-- | the arg to a InfixPredicateApplication can be either a NamedElement:ID or Expr
parsePredicateAppArg :: A.Object -> Parser Expr
parsePredicateAppArg arg =
  if has (ix "$type") arg
  then parseExpr arg
  else parseBareRefToVar (coerce arg)

parsePredicateApp ::  A.Object -> Parser Expr
parsePredicateApp predApp = do
    predicate <- parseExpr =<< predApp .: "predicate"
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

parseIntegerLiteral :: A.Object -> Parser Expr
parseIntegerLiteral literalNode = do
    let literalVal = literalNode ^? ix "value" % _String % _Integer % to fromInteger
    case literalVal of
      Just litVal -> pure . Lit $ IntLit litVal
      Nothing -> throwError $ "Failed to parse integer value. Node: " <> ppShow literalNode

parseLiteral :: FromJSON t => (t -> Lit) -> A.Object -> Parser Expr
parseLiteral literalExprCtor literalNode = do
    literalVal <- literalNode .: "value"
    return $ Lit $ literalExprCtor literalVal

{----------------------
    Utils
-----------------------}

tryParseTransparency :: A.Object -> Parser Transparency
tryParseTransparency (getTransparency -> Just "Opaque") = pure Opaque
tryParseTransparency (getTransparency -> Just "Transparent") = pure Transparent
tryParseTransparency _ = pure Transparent

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
  unique <- refPathToUnique nodePath
  pure $ MkName name unique

getValueFieldOfNode :: (JoinKinds (IxKind s) A_Prism k, Is k An_AffineFold, Ixed s,  A.AsValue (IxValue s), IsString (Index s)) => s -> A.Object
getValueFieldOfNode node = node `objAtKey` "value"
