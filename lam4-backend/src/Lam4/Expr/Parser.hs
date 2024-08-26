{- | 
  Aug 26 2024: This parser hasn't yet been updated with all the recent constructs and changes in the Langium grammar. 
  So don't expect to be able to parse into the backend concrete syntax right now.

  Parses the JSON representation of (the concrete syntax of)
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
import qualified Data.Set                 as Set
-- import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable            as F
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

newtype Ref = MkRef A.Object
  deriving newtype (Eq, Show, Ord, FromJSON)

parseRefToVar :: Ref -> Parser Expr
parseRefToVar ref = Var <$> relabelRef ref

relabelBareRef :: Ref -> Parser Name
relabelBareRef = relabelRefHelper getRef getRefText
  where
    getRef node = node ^? ix "$ref" % _String
    getRefText node = node ^? ix "$refText" % _String

{- | Relabel an object that is a ('wrapped') `Ref` to a `Name` -}
relabelRef :: Ref -> Parser Name
relabelRef = relabelRefHelper getRef getRefText
  where
    getRef node = node ^? ix "value" % ix "$ref" % _String
    getRefText node = node ^? ix "value" % ix "$refText" % _String

relabelRefHelper :: (A.Object -> Maybe RefPath) -> (A.Object -> Maybe Text) -> Ref -> Parser Name
relabelRefHelper getRef getRefText (MkRef node) = do
  let refPath = getRef node
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

  {- Make Env of nodePaths => Uniques
    All that's needed, for now, is *some* canonical order on the Uniques
  -}
  setEnv (zip nodePaths [1 .. ])
  parseDecls elementObjects

mkDecl :: Text -> Name -> Expr -> Decl
mkDecl typeOfNode name expr =
  if has (contains typeOfNode) recursiveTypes
  then Rec name expr
  else NonRec name expr

parseDecls :: [A.Object] -> Parser [Decl]
parseDecls = traverse parseDecl

parseDecl :: A.Object -> Parser Decl
parseDecl obj = do
  expr <- parseExpr obj
  exprType <- obj .: "$type"
  name <- getName obj
  pure $ mkDecl exprType name expr

{----------------------
    parseExpr
-----------------------}

parseExpr :: A.Object -> Parser Expr
parseExpr node = do
  (node .: "$type" :: Parser Text) >>= \case
    "Ref"            -> parseRefToVar            (coerce node)

    "SigDecl"        -> parseSigE           node

    -- literals
    "IntLit" -> parseIntegerLiteral node
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

    -- Note: Join is in the process of being disabled / deprecated
    "Join"           -> parseJoin           node

    typestr          -> throwError $ T.unpack typestr <> " not yet implemented"


{----------------------
    Relation related
-----------------------}
-- TODO: Adapt these to records

parseBuiltinTypeForRelation :: Text -> Parser BuiltinTypeForRelation
parseBuiltinTypeForRelation = \case
    "Integer" -> pure BuiltinTypeInteger
    "String"  -> pure BuiltinTypeString
    "Boolean" -> pure BuiltinTypeBoolean
    other     -> throwError ("Unexpected type " <> T.unpack other)

parseRelatum :: A.Object -> Parser Relatum
parseRelatum node = do
  (node .: "$type" :: Parser Text) >>= \case
    "CustomTypeDef" -> do
      name <- relabelBareRef $ coerce $ node `objAtKey` "annot"
      pure $ CustomType name
    "BuiltinType"   -> do
      builtinType <- parseBuiltinTypeForRelation =<< (node .: "annot" :: Parser Text)
      pure $ BuiltinType builtinType
    _               -> throwError "unrecognized relatum"

parseRelation :: Name -> A.Object -> Parser Expr
parseRelation parentSigName relationNode = do
    relationName <- getName relationNode
    relatum     <- parseRelatum =<< relationNode .: "relatum"
    description <- relationNode .:? "description"
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
  parents   <- traverse (relabelRef . MkRef) parentRefNodes
  sigName   <- getName sigNode
  relations <- traverse (parseRelation sigName) relationNodes
  pure $ Sig parents relations


{-----------------------------------
    BinExpr, UnaryExpr, IfThenElse
------------------------------------}

parseBinExpr :: A.Object -> Parser Expr
parseBinExpr node = do
  op    <- parseBinOp =<< node .: "op"
  left  <- parseExpr =<< node .: "left"
  right <- parseExpr =<< node .: "right"
  pure $ BinExpr op left right

parseJoin ::  A.Object -> Parser Expr
parseJoin node = do
  left <- parseExpr =<< node .: "left"
  right <- parseExpr =<< node .: "right"
  pure $ Join left right

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

type Binding = (Name, Expr)

parseLet :: A.Object -> Parser Expr
parseLet obj = do
  rows <- traverse parseVarDecl $ obj `getObjectsAtField` "vars"
  body <- parseExpr $ obj `objAtKey` "body"
  makeNestedLet body rows
    where
      makeNestedLet :: Expr -> [Binding] -> Parser Expr
      makeNestedLet body rows = F.foldrM nestLet body rows

      nestLet :: Binding -> Expr -> Parser Expr
      nestLet (name, valE) accE = pure $ Let name valE accE

parseVarDecl :: A.Object -> Parser Binding
parseVarDecl varDecl = do
  name <- getName varDecl
  val <- parseExpr (getValueFieldOfNode varDecl)
  pure (name, val)


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
  body <- parseExpr =<< anonFun .: "body"
  pure $ Fun paramNames body Nothing
    where
      anonFunParams = anonFun ^.. ix "params" % values % cosmos % ix "param" % _Object

parseFunE :: A.Object -> Parser Expr
parseFunE fun = do
  paramNames <- traverse parseParam (fun `getObjectsAtField` "params")
  body <- parseExpr =<< fun .: "body"
  originalRuleRef <- extractOriginalRuleRef fun
  pure $ Fun paramNames body originalRuleRef

{- | Treating predicate exprs separately from function expressions,
even though the current code is similar,
because predicates will likely be treated differently in symbolic execution
(and hence the specifications for functions versus predicates are likely to diverge)
-}
parsePredicateE :: A.Object -> Parser Expr
parsePredicateE predicate = do
  paramNames <- traverse parseParam (getPredicateParams predicate)
  body <- parseExpr =<< predicate .: "body"
  originalRuleRef <- extractOriginalRuleRef predicate
  pure $ Predicate paramNames body originalRuleRef
    where
      getPredicateParams predicateNode = predicateNode ^.. ix "params" % values % ix "param" % _Object

parsePredicateApp ::  A.Object -> Parser Expr
parsePredicateApp predApp = do
    predicate <- parseExpr =<< predApp .: "predicate"
    args <- traverse parseExpr (predApp `getObjectsAtField` "args")
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
