concrete Lam4Eng of Lam4 = open Prelude, Coordination in {
  lincat
    -- TypeDecl
    -- RowTypeDecl
    [RowTypeDecl] = {s : Str ; isEmpty : IsEmpty} ;
    Metadata = LinMetadata ;
    ListExpr = ListX0 ;
    ListName = ListX0 ;
    BinOp = {s : Verbosity => Str} ;
  param
    IsEmpty = Empty | NonEmpty ;
    MyListSize = Zero | One | Many ;
    Verbosity = Concise | Verbose ;

  oper
    ListX0 : Type = ListX ** {size : MyListSize} ;

    baseListX0 : ListX0 = {
      s1, s2 = "" ;
      size = Zero
    } ;

    consListX0 : SS -> ListX0 -> ListX0 = \x,xs -> case xs.size of {
        Many => xs ** {
          s1 = x.s ++ bindComma ++ xs.s1
          } ;
        One => xs ** {
          s1 = x.s ;
          size = Many
          } ;
        Zero => xs ** {
          s2 = x.s ;
          size = One
          }
      } ;

    LinMetadata : Type = {s : Str ; isEmpty : IsEmpty} ;
    linRowMd : LinMetadata -> Str = \md ->
      case md.isEmpty of {
        Empty => md.s ;
        NonEmpty => "; i.e." ++ md.s
      } ;
    linTypeDeclMd : LinMetadata -> Str = \md ->
      case md.isEmpty of {
        Empty => "is a class" ++ md.s ;
        NonEmpty => "is" ++ md.s
      } ;
    linType : SS -> Str = \s -> "of type" ++ s.s ;

  lin
    -- Placeholder, or skip some constructs?
    EmptyS = {s = ""} ;
    TypeDeclS td = td ;
    ExprS expr = expr ;
    EvalS expr = {s = "evaluate" ++ expr.s ++ linebreak} ;
    EvalWhetherS expr = {s = "evaluate whether" ++ expr.s ++ linebreak} ;
    AssignS name expr = {
      s = quote name.s
       ++ "is calculated by"
       ++ indent1 expr.s
       ++ linebreak } ;
    LetIsTrue name expr = {
      s = quote name.s
       ++ "is true if"
       ++ indent1 expr.s
       ++ linebreak
       } ;
    AtomicConcept name = {s = name.s ++ "is an atomic concept."} ;

    -- Metadata
    MkMetadata str = str ** {isEmpty = NonEmpty} ;
    NoMetadata = {s = [] ; isEmpty = Empty} ;

    -- Type declarations
    MkRowTypeDecl md field typ = {s = bullet ++ "its" ++ field.s ++ linType typ ++ linRowMd md} ;
    MkRowDecl md field = {s = bullet ++ "its" ++ field.s ++ linRowMd md} ;

    -- These funs are automatically generated from cat [RowTypeDecl]{0} ;
    -- : [RowTypeDecl]
    BaseRowTypeDecl = {s = [] ; isEmpty = Empty} ;
    -- : RowTypeDecl -> [RowTypeDecl] -> [RowTypeDecl]
    ConsRowTypeDecl t ts =
      let sep : Str = case ts.isEmpty of {
                        Empty => [] ;
                        NonEmpty => linebreak } ;
       in {s = tab ++ t.s ++ sep ++ ts.s ; isEmpty = NonEmpty} ;

    MkTypeDecl md name rtds = {
      s = artIndef ++ name.s ++ linTypeDeclMd md ++ "." ++
          case rtds.isEmpty of {
            Empty => [] ;
            NonEmpty => "Each" ++ name.s ++
                     "has associated with it information like" ++
                     linebreak ++ rtds.s
          } ;
      } ;

  oper
    bullet = "*" ;
    -- just ad hoc characters to `tr "°∞" "\t\n"` in shell
    tab = "°°" ;
    space = "°" ;
    linebreak = "∞" ;

    indent1, indent2 : Str -> Str ;
    indent1 str = linebreak ++ space ++ str ;
    indent2 str = linebreak ++ tab ++ str ;

    quote : Str -> Str ;
    quote str = "[" ++ BIND ++ str ++ BIND ++ "]" ;

    quoteSS : SS -> SS ;
    quoteSS ss = {s = quote ss.s} ;

    artIndef = pre {
      "eu" | "Eu" | "uni" | "Uni" => "A" ;
      "un" | "Un" => "An" ;
      "a" | "e" | "i" | "o" | "A" | "E" | "I" | "O" => "An" ;
      _ => "A"
      } ;

    conjXss = overload {
      conjXss : ListX0 -> SS = \xs -> ss (conjX "" xs) ;
      conjXss : Str -> ListX0 -> SS = \c,xs -> ss (conjX c xs)
    } ;

    conjX : Str -> ListX0 -> Str = \conj,xs -> case xs.size of {
      Many => conjunctX (ss conj) xs ;
      _ => xs.s2
      } ;
    linArgs = overload {
      linArgs : Str -> ListX0 -> Str = \s,xs ->
        case xs.size of {
          Zero => "" ;
          _    => s ++ conjX "and" xs } ;
      linArgs : Str -> ListX0 -> Str -> Str = \pr,xs,pst ->
        case xs.size of {
          Zero => "" ;
          _    => pr ++ conjX "and" xs ++ pst }
      } ;

    mkBinExpr : Str -> Str -> {s : Verbosity => Str} ;
    mkBinExpr short long = {
      s = table {
        Concise => short ;
        Verbose => long }
      } ;

  lin

    -- Expressions
    MkName str = str ;

    QuoteVar name = {s = quote name.s} ;
    Var name = name ;
    Lit name = name ;
    Unary op expr = cc2 op expr ;
    BinExpr op e1 e2 = {
      s = e1.s
       ++ op.s ! Concise
       ++ e2.s
      } ;

    QuotedBinExpr op e1 e2 = {
      s = quote e1.s
       ++ op.s ! Concise
       ++ quote e2.s
      } ;

    VerboseBinExpr op e1 e2 = {
      s =         (quote e1.s)
       ++ indent1 (op.s ! Verbose)
       ++ indent1 (quote e2.s)
      } ;

    Unknown expr = {
      s = quote expr.s ++ "is unknown"
    } ;

    Uncertain expr = {
      s = quote expr.s ++ "is uncertain"
    } ;


    IfThenElse if then else = {
      s = "if" ++ if.s
        ++ indent2 "then" ++ then.s
        ++ indent2 "else" ++ else.s
      } ;

    -- : Expr -> Expr -> Expr ;
    InstanceSumIf entities condition = {
      s = "adding up those of"
       ++ indent2 (quote entities.s)
       ++ indent1 "where"
       ++ indent2 (quote condition.s)
      } ;

    -- : Expr -> [Expr] -> Expr ;
    FunApp f xs = {s = f.s ++ linArgs "of" xs} ;
    -- Record : (Row Expr) -> Expr ;               -- record construction

    -- : Expr -> Name -> Expr ;             -- record projection
    Project rec field = {s = glue rec.s "'s" ++ field.s} ;

    -- : Name -> Metadata -> [Name] -> Expr -> Expr ;  -- Function
    Fun funname md args body = {
      s = "Function" ++ funname.s ++ ":" ++ linebreak
       ++ linArgs "given" args ", return"
       ++ indent1 body.s
      } ;

    -- : S -> Expr -> Expr ;
    Let decl expr = {
      s = decl.s ++ linebreak ++ expr.s
    } ;

    -- : Name -> Expr -> Expr ;
    Record name field = {s = glue name.s "'s" ++ field.s} ;
    Sig parents relations = cc2 (conjXss parents) (conjXss relations) ;
    -- Let :        Decl Expr
    -- StatementBlock :  (NonEmpty Statement)

    NormIsInfringed name = {s = name.s ++ "is infringed"} ;

    -- : (predname : Name)  Metadata -> (args : [Name]) -> Expr -> Expr ;
    Predicate is_eligible md args body = {
      s = case args.size of {
            -- single argument, "args is_eligible"
            One => conjX "" args
                ++ glue is_eligible.s "," ;
            -- multiple arguments, "is_eligible holds"
            _   => quote is_eligible.s
                ++ linArgs "holds for" args ","
          } ++ "if" ++ body.s ;
      } ;

    -- : Expr -> [Expr] -> Expr ;
    PredApp = FunApp ;

    PredAppMany op args preds = {
      s = quote (conjX "and" args) ++ "is" ++ conjX (op.s ! Verbose) preds
    } ;



    Fold combine nil over = {
      s = linebreak ++
      "Combine" ++ quote over.s ++ "into one value," ++ linebreak
       ++ "using the function" ++ quote combine.s ++ "," ++ linebreak
       ++ "starting from" ++ nil.s
    } ;

    -- When generating natural language for some file that defines a bunch of stuff like cons, map, filter,
    -- apply this function instead to keep it in the AST
    -- but skip linearization.
    -- : Name -> Expr
    KnownFunction _ = {s = ""} ;

    -- Unary and binary operators
    Not = ss "not" ;
    Floor = ss "floor of" ;
    Ceiling = ss "ceiling of" ;
    IntegerToFraction = ss "" ; -- not important in NLG
    UnaryMinus = ss "-" ;

    Or = mkBinExpr "||" "or" ;
    And = mkBinExpr "&&" "and" ;
    Plus = mkBinExpr "+" "added to" ;
    Minus = mkBinExpr "-" "subtracted from" ;
    Modulo = mkBinExpr "%" "modulo" ;
    Mult = mkBinExpr "*" "multiplied by" ;
    Divide = mkBinExpr "/" "divided by" ;
    Lt = mkBinExpr "<" "is less than" ;
    Le = mkBinExpr "≤" "is less than or equal" ;
    Gt = mkBinExpr ">" "is greater than" ;
    Ge = mkBinExpr "≥" "is greater than or equal to" ;
    Eq = mkBinExpr "=" "equals to" ;
    Ne = mkBinExpr "≠" "is not equal to" ;

    BaseExpr, BaseName = baseListX0 ;
    ConsExpr, ConsName = consListX0 ;
    ConjExpr = conjXss ;
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


A/An ‘Lottery’ is <insert description>.
Each lottery has associated with it information like
  * its ‘total jackpot’; i.e., how much can be won from the jackpot
  * its ‘tax deductible status’; i.e., whether buying tickets from this lottery is tax deductible
-}