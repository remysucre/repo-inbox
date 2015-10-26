concrete IdiomFre of Idiom = CatFre ** 
  open (P = ParamX), PhonoFre, MorphoFre, ParadigmsFre, Prelude in {

  flags optimize=all_subs ;

  lin
    ImpersCl vp = mkClause "il" True False (agrP3 Masc Sg) vp ;
    GenericCl vp = mkClause "on" True False (agrP3 Masc Sg) vp ;

    ExistNP np = 
      mkClause "il" True False (agrP3 Masc Sg)
        (insertClit3 "y" (insertComplement (\\_ => (np.s ! Acc).ton) (predV avoir_V))) ;

    ExistIP ip = {
      s = \\t,a,p,_ => 
        ip.s ! Nom ++ 
        (mkClause "il" True False (agrP3 Masc Sg)
              (insertClit3 "y" (predV avoir_V))).s 
           ! DDir ! t ! a ! p ! Indic ---- DInv
      } ;

    CleftNP np rs = mkClause elisCe True np.isPol (agrP3 Masc Sg) 
      (insertComplement (\\_ => rs.s ! Indic ! np.a)
        (insertComplement (\\_ => (np.s ! rs.c).ton) (predV copula))) ;

    CleftAdv ad s = mkClause elisCe True False (agrP3 Masc Sg) 
      (insertComplement (\\_ => conjThat ++ s.s ! Indic)
        (insertComplement (\\_ => ad.s) (predV copula))) ;


    ProgrVP vp = 
      insertComplement 
        (\\a => "en" ++ "train" ++ elisDe ++ infVP vp a) 
        (predV copula) ;

    ImpPl1 vp = {
      s = mkImperative False P1 vp ! Pos ! Masc ! Pl  --- fem
      } ;

    ImpP3 np vp = {
      s = (mkClause (np.s ! Nom).comp np.hasClit False np.a vp).s 
             ! DInv ! RPres ! Simul ! Pos ! Conjunct
      } ;

  oper
    elisCe = elision "c" ;

}


