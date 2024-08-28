concrete Lam4Eng of Lam4 = {
  lincat
    -- RecordDecl
    -- TypeAnnot
    [TypeAnnot] = {s : Str ; isEmpty : IsEmpty} ;

  param
    IsEmpty = Empty | NonEmpty ;

  lin
    MkTypeAnnot field _typ = {s = bullet ++ "its" ++ field.s} ;

    MetadataTypeAnnot md annot = {s = annot.s ++ "; i.e." ++ md.s} ;

    -- These funs are automatically generated from cat [TypeAnnot]{0} ;
    -- : [TypeAnnot]
    BaseTypeAnnot = {s = [] ; isEmpty = Empty} ;
    -- : TypeAnnot -> [TypeAnnot] -> [TypeAnnot]
    ConsTypeAnnot t ts =
      let sep : Str = case ts.isEmpty of {
                        Empty => [] ;
                        NonEmpty => linebreak } ;
       in {s = tab ++ t.s ++ sep ++ ts.s ; isEmpty = NonEmpty} ;

    MkRecordDecl name typeannots = {
      s = artIndef ++ name.s ++ "is a class." ++
          case typeannots.isEmpty of {
            Empty => [] ;
            NonEmpty => "Each" ++ name.s ++
                     "has associated with it information like" ++
                     linebreak ++ typeannots.s
          } ;
      } ;

    MetadataRecordDecl name md typeannots = {
      s = artIndef ++ name.s ++ "is" ++ md.s ++ "." ++
          case typeannots.isEmpty of {
            Empty => [] ;
            NonEmpty => "Each" ++ name.s ++
                     "has associated with it information like" ++
                     linebreak ++ typeannots.s
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