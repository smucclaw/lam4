abstract Lam4 = {
  flags startcat = S ;
  cat
    S ;
    TypeDecl ;
    RowTypeDecl ;
    [RowTypeDecl]{0} ;
    Metadata ;

    Expr ;
    [Expr]{0} ;
    UnaryOp ;
    BinOp ;
    Name ;
    [Name]{0} ;

  fun
    -- Placeholder, or skip some constructs?
    EmptyS : S ;
    TypeDeclS : TypeDecl -> S ;
    ExprS : Expr -> S ;

    -- Metadata
    MkMetadata : String -> Metadata ;
    NoMetadata : Metadata ; -- empty

    -- Type declarations
    MkRowTypeDecl : Metadata -> (fld : Name) -> (typ : Name) -> RowTypeDecl ;
    MkRowDecl : Metadata -> (fld : Name) -> RowTypeDecl ; -- no type

    MkTypeDecl : Metadata -> (name : Name) -> [RowTypeDecl] -> TypeDecl ;

    -- Expressions
    MkName : String -> Name ;

    Var : Name -> Expr ;
  -- | Lit        Lit
  -- | Cons       Expr Expr                           -- list cons
  -- | List       [Expr]                              -- construct a list
    Unary   : UnaryOp -> Expr -> Expr ;
    BinExpr : BinOp -> Expr -> Expr -> Expr ;
    IfThenElse : Expr -> Expr -> Expr -> Expr ;
  -- TODO: Not Yet Implemented / need to think more about what collection types to support
  -- TODO: Add Cons after have wired up to an evaluator
  -- | ListExpr   ListOp [Expr]
    FunApp : Expr -> [Expr] -> Expr ;
    -- Record : (Row Expr) -> Expr ;               -- record construction
    Project : Expr -> Name -> Expr ;             -- record projection
    Fun : (funname : Name) -> Metadata -> (args : [Name]) -> Expr -> Expr ;  -- Function
    -- Let :        Decl Expr
    -- StatementBlock :  (NonEmpty Statement)

    NormIsInfringed : Name -> Expr ;             -- NormIsInfringed NameOfNorm.
                                                   -- This is a predicate that checks if @nameOfNorm@ is violated (users can supply unique identifiers for Deontics and IfThenOtherwise statements that contain a Deontic)


    Predicate : (predname : Name) -> Metadata -> (args : [Name]) -> Expr -> Expr ;            -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
    PredApp : Expr -> [Expr] -> Expr ;

    -- Unary and binary operators
    Not,
    Floor,
    Ceiling,
    UnaryMinus,
    IntegerToFraction : UnaryOp ;

    Or : BinOp ;
    And : BinOp ;
    Plus : BinOp ;
    Minus : BinOp ;
    Modulo : BinOp ;   -- ^ (integer) remainder
    Mult : BinOp ;
    Divide : BinOp ;
    Lt : BinOp ;
    Le : BinOp ;
    Gt : BinOp ;
    Ge : BinOp ;      -- ^ greater-than-or-equal
    Eq : BinOp ;      -- ^ equality (of Booleans, numbers or atoms)
    Ne : BinOp ;      -- ^ inequality (of Booleans, numbers or atoms)
}

{-
— <some kind of description>
LOTTERY {
  — how much can be won from the jackpot
 `total jackpot`: Integer,
   — whether buying tickets from this lottery is tax deductible
  `tax deductible status`: Boolean
}


This can be rendered as:

MkTypeDecl (MkMetadata "a game where you lose money") "Lottery" (ConsRowTypeDecl (MkRowDecl (MkMetadata "how much can be won from the jackpot") "‘total jackpot’") (ConsRowTypeDecl (MkRowDecl (MkMetadata "whether buying tickets from this lottery is tax deductible") "‘tax deductible status’") BaseRowTypeDecl))

A/An ‘Lottery’ is <insert description>.
Each lottery has associated with it information like
  * its ‘total jackpot’; i.e., how much can be won from the jackpot
  * its ‘tax deductible status’; i.e., whether buying tickets from this lottery is tax deductible
-}