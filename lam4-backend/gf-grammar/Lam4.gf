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

    -- Special values for aggregation
    ListOp ;
    LExpr ;
    [LExpr]{2} ;

    -- (condition, value)
    -- in order to flatten nested if-then-elses into if-elif-elif-…-else
    IfThen ;
    [IfThen]{1} ;
  fun
    -- Placeholder, or skip some constructs?
    EmptyS : S ;
    TypeDeclS : TypeDecl -> S ;
    EvalS,
    EvalWhetherS,
    ExprS : Expr -> S ;
    AssignS : Name -> Expr -> S ;
    LetIsTrue : Name -> Expr -> S ;
    AtomicConcept : Name -> S ;

    -- Metadata
    MkMetadata : String -> Metadata ;
    NoMetadata : Metadata ; -- empty

    -- Type declarations
    MkRowTypeDecl : Metadata -> (fld : Name) -> (typ : Name) -> RowTypeDecl ;
    MkRowDecl : Metadata -> (fld : Name) -> RowTypeDecl ; -- no type

    MkTypeDecl : Metadata -> (name : Name) -> [RowTypeDecl] -> TypeDecl ;

    -- Expressions
    MkName : String -> Name ;
    Lit,
    QuoteVar,
    Var : Name -> Expr ;

    -- the following correspond to List and ListExpr in Lam4 AST,
    -- named differently because of a bug in GF,
    -- see https://github.com/GrammaticalFramework/gf-core/issues/163
    ConjExpr : [Expr] -> Expr ; -- construct a list

    coerceListExpr : Expr -> LExpr ;
    ApplyListOp : ListOp -> [LExpr] -> Expr ;

    ListAnd, ListOr : ListOp ;

    Unary   : UnaryOp -> Expr -> Expr ;
    VerboseBinExpr, -- newline + quotes around args
    QuotedBinExpr, -- no newline, quotes around args
    BinExpr : BinOp -> Expr -> Expr -> Expr ;

    Known, Certain, Unknown, Uncertain : Expr -> Expr ;

    -- TODO: get rid of all nested IfThenElses
    IfThenElse : Expr -> Expr -> Expr -> Expr ;
    -- make them into Elif instead
    Elif: [IfThen] -> Expr -> Expr ;
    {-

      IF    i's `the business's number of past and current clients`  is larger than
       * 10.0: => the business's client bonus factor is 1.5
       * 4.0  => the business's client bonus factor is 1.35
       * 1.0  => the business's client bonus factor is 1.2
       * ELSE => the business's client bonus factor is 1.0

    -}

{- [The investor's total allocation in Energy]
   is calculated by (AssignS)
   adding up (InstanceSumIf)
    [the investor's funds allocated to the investment as per sector share]
   where
     [the investment is in Energy]
-}
    InstanceSumIf : (entities, condition : Expr)-> Expr ;
    InstanceSum : (entities : Expr) -> Expr ;
    FunApp : Expr -> [Expr] -> Expr ;
    -- Record : (Row Expr) -> Expr ;               -- record construction
    Project : Expr -> Name -> Expr ;             -- record projection
    Fun : (funname : Name) -> Metadata -> (args : [Name]) -> Expr -> Expr ;  -- Function
    Sig : [Name] -> [Expr] -> Expr ; -- TODO: what is this? only `Sig [] []` present in royalflush data

    {- TODO: this is used in context like

       LET { foo
           , bar
           , baz
           }
       IN { expression that uses definitions
          , another expression
          , yet another
          … }

       as AST it looks like
         Let (foo
           Let (bar
             Let baz )))

       transform into:

       [all variables]

       [all Exprs]

       and linearize as
       -- Definitions ---
       foo = …
       bar = …
       baz = …

       -- Expressions --
       if they are just like
       `the business eligibility text` = `the business eligibility text`,
       then don't print them.
    -}
    Let : S -> Expr -> Expr ;
    Record : Name -> Expr -> Expr ;
    -- StatementBlock :  (NonEmpty Statement)

    NormIsInfringed : Name -> Expr ;             -- NormIsInfringed NameOfNorm.
                                                   -- This is a predicate that checks if @nameOfNorm@ is violated (users can supply unique identifiers for Deontics and IfThenOtherwise statements that contain a Deontic)


    Predicate : (predname : Name) -> Metadata -> (args : [Name]) -> Expr -> Expr ;            -- Differs from a function when doing symbolic evaluation. Exact way in which they should differ is WIP.
    PredApp : Expr -> [Expr] -> Expr ;
    PredAppMany : BinOp -> (business_is_good : [Expr]) -> (uncertain_unknown : [Expr]) -> Expr ;
    Fold : Expr -> Expr -> Expr -> Expr ;

    -- When generating natural language for some file that defines a bunch of stuff like cons, map, filter,
    -- apply this function instead to keep it in the AST
    -- but skip linearization.
    KnownFunction : Name -> Expr ;


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