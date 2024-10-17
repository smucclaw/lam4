concrete Lam4Eng of Lam4 = open Prelude, Coordination in {
  lincat
    -- TypeDecl
    -- RowTypeDecl
    [RowTypeDecl] = {s : Str ; isEmpty : IsEmpty} ;
    Metadata = LinMetadata ;
    ListExpr = ListX0 ;
    ListName = ListX0 ;
  param
    IsEmpty = Empty | NonEmpty ;
    MyListSize = Zero | One | Many ;

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
    EvalS expr = {s = "evaluate" ++ expr.s} ;
    EvalWhetherS expr = {s = "evaluate whether" ++ expr.s} ;
    AssignS name expr = {s = quote name.s ++ "is assigned the value" ++ quote expr.s} ;

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
    tab = "°" ;
    linebreak = "∞" ;

    quote : Str -> Str ;
    quote str = "‘" ++ BIND ++ str ++ BIND ++ "’" ;


    artIndef = pre {
      "eu" | "Eu" | "uni" | "Uni" => "A" ;
      "un" | "Un" => "An" ;
      "a" | "e" | "i" | "o" | "A" | "E" | "I" | "O" => "An" ;
      _ => "A"
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


  lin

    -- Expressions
    MkName str = str ;

    QuoteVar name = {s = quote name.s} ;
    Var name = name ;
    Lit name = name ;
    Unary op expr = cc2 op expr ;
    BinExpr op e1 e2 = cc3 e1 op e2 ;

    IfThenElse if then else = {
      s = "if" ++ if.s ++ linebreak ++ tab
        ++ "then" ++ then.s ++ linebreak ++ tab
        ++ "else" ++ else.s
      } ;

    -- : Expr -> [Expr] -> Expr ;
    FunApp f xs = {s = f.s ++ linArgs "of" xs} ;
    -- Record : (Row Expr) -> Expr ;               -- record construction

    -- : Expr -> Name -> Expr ;             -- record projection
    Project rec field = {s = glue rec.s "'s" ++ field.s} ;

    -- : Name -> Metadata -> [Name] -> Expr -> Expr ;  -- Function
    Fun funname md args body = {
      s = "Function" ++ funname.s ++ ":" ++ linebreak
       ++ linArgs "given" args ","
       ++ "return" ++ body.s
      } ;
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
    Fold combine nil over = {
      s = linebreak ++
      "Combine" ++ quote over.s ++ "into one value," ++ linebreak
       ++ "using the function" ++ quote combine.s ++ "," ++ linebreak
       ++ "starting from" ++ nil.s
    } ;

    -- Unary and binary operators
    Not = ss "not" ;
    Floor = ss "floor of" ;
    Ceiling = ss "ceiling of" ;
    IntegerToFraction = ss "" ; -- not important in NLG
    UnaryMinus = ss "-" ;

    Or = ss "or" ;
    And = ss "and" ;
    Plus = ss "+" ;
    Minus = ss "-" ;
    Modulo = ss "%" ;
    Mult = ss "*" ;
    Divide = ss "/" ;
    Lt = ss "<" ;
    Le = ss "≤" ;
    Gt = ss ">" ;
    Ge = ss "≥" ;
    Eq = ss "equals to" ;
    Ne = ss "≠" ;

    BaseExpr, BaseName = baseListX0 ;
    ConsExpr, ConsName = consListX0 ;
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