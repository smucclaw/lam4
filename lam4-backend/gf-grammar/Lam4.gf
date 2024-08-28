abstract Lam4 = {
  flags startcat = RecordDecl ;
  cat
    RecordDecl ;
    TypeAnnot ;
    [TypeAnnot]{0} ;
  fun
    MkTypeAnnot : (fld : String) -> (typ : String) -> TypeAnnot ;

    MetadataTypeAnnot : (md : String) -> TypeAnnot -> TypeAnnot ;

    MkRecordDecl : (name : String) -> [TypeAnnot] -> RecordDecl ;

    MetadataRecordDecl : (name : String) -> (md : String) -> [TypeAnnot] -> RecordDecl ;


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