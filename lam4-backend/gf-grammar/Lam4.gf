abstract Lam4 = {
  flags startcat = TypeDecl ;
  cat
    TypeDecl ;
    RowTypeDecl ;
    [RowTypeDecl]{0} ;
    Metadata ;
  fun
    MkMetadata : String -> Metadata ;
    NoMetadata : Metadata ; -- empty

    MkRowTypeDecl : Metadata -> (fld : String) -> (typ : String) -> RowTypeDecl ;
    MkRowDecl : Metadata -> (fld : String) -> RowTypeDecl ; -- no type

    MkTypeDecl : Metadata -> (name : String) -> [RowTypeDecl] -> TypeDecl ;

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