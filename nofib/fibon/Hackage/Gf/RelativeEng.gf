concrete RelativeEng of Relative = CatEng ** open ResEng in {

  flags optimize=all_subs ;

  lin

    RelCl cl = {
      s = \\t,a,p,_ => "such" ++ "that" ++ cl.s ! t ! a ! p ! ODir ; 
      c = Nom
      } ;

    RelVP rp vp = {
      s = \\t,ant,b,ag => 
        let 
          agr = case rp.a of {
            RNoAg => ag ;
            RAg a => a
            } ;
          cl = mkClause (rp.s ! RC (fromAgr agr).g Nom) agr vp
        in
        cl.s ! t ! ant ! b ! ODir ;
      c = Nom
      } ;

-- Pied piping: "at which we are looking". Stranding and empty
-- relative are defined in $ExtraEng.gf$ ("that we are looking at", 
-- "we are looking at").

    RelSlash rp slash = {
      s = \\t,a,p,agr => 
          slash.c2 ++ rp.s ! RPrep (fromAgr agr).g ++ slash.s ! t ! a ! p ! ODir ;
      c = Acc
      } ;

    FunRP p np rp = {
      s = \\c => np.s ! Acc ++ p.s ++ rp.s ! RPrep (fromAgr np.a).g ;
      a = RAg np.a
      } ;

    IdRP = 
     { s = table {
        RC _ Gen => "whose" ; 
        RC Neutr _  => "which" ;
        RC _ Acc    => "whom" ;
        RC _ Nom    => "who" ;
        RPrep Neutr => "which" ;
        RPrep _     => "whom"
        } ;
      a = RNoAg
      } ;

}
