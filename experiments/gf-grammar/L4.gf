abstract L4 = {
  flags startcat = Rule ;
  cat
    Rule ;

    Subject ;
    Who ;
    Action ;
    Definition ;
    [Definition]{2} ;

  fun
    Hornlike : String -> Definition -> Rule ;

    -- Finaldefinitions
    Pre : String -> [Definition] -> [Definition] ;
    PrePost : String -> String -> [Definition] -> [Definition] ;
    MkDefinition : String -> Definition ;
    And, Or : [Definition] -> Definition ;

    Regulative : Subject -> Who -> Action -> Rule ;

    -- stuff for regulative
    MkSubject : String -> Subject ;
    MkWho : String -> Who ;
    MUST, MAY, SHALL, SHANT : String -> Action ;


}