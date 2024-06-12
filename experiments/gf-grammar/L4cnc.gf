concrete L4cnc of L4 = {
  lincat
    -- Rule ;

    -- Subject ;
    -- Who ;
    -- Action ;
    FinalDefinition,
    Definition = {
      s : Str ;
      ncols --, nrows
       : HowMany} ;
    ListDefinition = {
      s1 : Str ;
      s2 : CONJ => Str ;
      post : Str ;
      ncols --, nrows
       : HowMany
      } ;

  param
    CONJ = AND | OR ;

    HowMany = One | Two | Three | Four ;
    -- could use Ints type from Prelude but this grammar is RGL-free

  oper
    increment : HowMany => HowMany = table {
      One => Two ;
      Two => Three ;
      _ => Four -- 4 is max here
    } ;

    t  : Str -> Str = \str -> "<table>" ++ str ++ "</table>" ;
    td : Str -> Str = \str -> "<td>" ++ str ++ "</td>" ;
    th : Str -> Str = \str -> "<th>" ++ str ++ "</th>" ;
    tr : Str -> Str = \str -> "<tr>" ++ str ++ "</tr>" ;

    emptyCol : Str -> Str = \str -> "<td></td>" ++ str ;
    emptyCols : HowMany => Str = table {
      One => emptyCol "" ;
      Two => emptyCol (emptyCol "") ;
      Three => emptyCol (emptyCol (emptyCol "")) ;
      Four => emptyCol (emptyCol (emptyCol (emptyCol "")))
    } ;


  lin
    -- : String -> FinalDefinition -> Rule ;
    Hornlike name defn = {
      s =  t
          (tr (th name.s ++ td "MEANS" ++ emptyCols ! defn.ncols)
        ++ defn.s)
    } ;

    -- : String -> [Definition] -> [Definition] ;
    Pre pr defn = defn ** {
      s1 = td pr.s ++ emptyCol defn.s1 ;
      s2 = \\conj => emptyCol (defn.s2 ! conj) ;
      ncols = increment ! defn.ncols ;
    } ;
    -- : String -> String -> [Definition] -> [Definition] ;
    PrePost pr pst defn = defn ** {
      s1 = td pr.s ++ defn.s1 ;
      s2 = \\conj => emptyCol (defn.s2 ! conj) ;
      post = tr ((td pst.s) ++ emptyCols ! defn.ncols) ;
      ncols = increment ! defn.ncols ;
    } ;

    MkDefinition str = {
      s = td str.s ;
      ncols = One
      };

    BaseDefinition d1 d2 = {
      s1 = emptyCol d1.s ;
      s2 = table {
            AND => td "AND" ++ d2.s ;
            OR => td "OR" ++ d2.s } ;
      post = "" ;
      ncols = Two ;
    } ;

    -- Simplification: no lists higher than 2
    -- ConsDefinition d ds = ds ** {
    --   s1 = linebreak d.s ds.s1 ;
    -- } ;

    -- : [Definition] -> Definition ;
    And defns = {
      s = tr defns.s1 ++ tr (defns.s2 ! AND) ++ defns.post ;
      ncols = increment ! defns.ncols ;
    } ;
    Or defns = {
      s = tr defns.s1 ++ tr (defns.s2 ! OR) ++ defns.post ;
      ncols = increment ! defns.ncols ;
    } ;

    -- Regulative : Subject -> Who -> Action -> Rule ;

    -- -- stuff for regulative
    -- MkSubject : String -> Subject ;
    -- MkWho : String -> Who ;
    -- MUST, MAY, SHALL, SHANT : String -> Action ;


}