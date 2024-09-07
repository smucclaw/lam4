concrete Lam4Eng of Lam4 = open Prelude in {
  lincat
    -- TypeDecl
    -- RowTypeDecl
    [RowTypeDecl] = {s : Str ; isEmpty : IsEmpty} ;
    Metadata = LinMetadata ;

  param
    IsEmpty = Empty | NonEmpty ;

  oper
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
    MkMetadata str = str ** {isEmpty = NonEmpty} ;
    NoMetadata = {s = [] ; isEmpty = Empty} ;

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


    artIndef = pre {
      "eu" | "Eu" | "uni" | "Uni" => "A" ;
      "un" | "Un" => "An" ;
      "a" | "e" | "i" | "o" | "A" | "E" | "I" | "O" => "An" ;
      _ => "A"
      } ;
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