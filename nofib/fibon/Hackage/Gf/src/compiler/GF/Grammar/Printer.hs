----------------------------------------------------------------------
-- |
-- Module      : GF.Grammar.Printer
-- Maintainer  : Krasimir Angelov
-- Stability   : (stable)
-- Portability : (portable)
--
-----------------------------------------------------------------------------

module GF.Grammar.Printer
           ( TermPrintQual(..)
           , ppLabel
           , ppModule
           , ppJudgement
           , ppTerm
           , ppPatt
           , ppValue
           , ppConstrs
           , ppPosition
           ) where

import GF.Infra.Ident
import GF.Infra.Modules
import GF.Infra.Option
import GF.Grammar.Values
import GF.Grammar.Grammar

import Text.PrettyPrint
import Data.Maybe (maybe, isNothing)
import Data.List  (intersperse)
import qualified Data.Map as Map

data TermPrintQual = Qualified | Unqualified

ppModule :: TermPrintQual -> SourceModule -> Doc
ppModule q (mn, ModInfo mtype mstat opts exts with opens _ jments) =
    hdr $$ nest 2 (ppOptions opts $$ vcat (map (ppJudgement q) defs)) $$ ftr
    where
      defs = Map.toList jments

      hdr = complModDoc <+> modTypeDoc <+> equals <+> 
            hsep (intersperse (text "**") $
                  filter (not . isEmpty)  $ [ commaPunct ppExtends exts
                                            , maybe empty ppWith with
                                            , if null opens
                                                then lbrace
                                                else text "open" <+> commaPunct ppOpenSpec opens <+> text "in" <+> lbrace
                                            ])

      ftr = rbrace

      complModDoc =
        case mstat of
          MSComplete   -> empty
          MSIncomplete -> text "incomplete"

      modTypeDoc =
        case mtype of
          MTAbstract         -> text "abstract"  <+> ppIdent mn
          MTResource         -> text "resource"  <+> ppIdent mn
          MTConcrete abs     -> text "concrete"  <+> ppIdent mn <+> text "of" <+> ppIdent abs
          MTInterface        -> text "interface" <+> ppIdent mn
          MTInstance int     -> text "instance"  <+> ppIdent mn <+> text "of" <+> ppIdent int

      ppExtends (id,MIAll        ) = ppIdent id
      ppExtends (id,MIOnly   incs) = ppIdent id              <+> brackets (commaPunct ppIdent incs)
      ppExtends (id,MIExcept incs) = ppIdent id <+> char '-' <+> brackets (commaPunct ppIdent incs)
      
      ppWith (id,ext,opens) = ppExtends (id,ext) <+> text "with" <+> commaPunct ppInstSpec opens

ppOptions opts = 
  text "flags" $$
  nest 2 (vcat [text option <+> equals <+> str value <+> semi | (option,value) <- optionsGFO opts])

ppJudgement q (id, AbsCat pcont ) =
  text "cat" <+> ppIdent id <+>
  (case pcont of
     Just (L _ cont) -> hsep (map (ppDecl q) cont)
     Nothing         -> empty) <+> semi
ppJudgement q (id, AbsFun ptype _ pexp) =
  (case ptype of
     Just (L _ typ) -> text (if isNothing pexp then "data" else "fun") <+> ppIdent id <+> colon <+> ppTerm q 0 typ <+> semi
     Nothing        -> empty) $$
  (case pexp of
     Just []  -> empty
     Just eqs -> text "def" <+> vcat [ppIdent id <+> hsep (map (ppPatt q 2) ps) <+> equals <+> ppTerm q 0 e <+> semi | L _ (ps,e) <- eqs]
     Nothing  -> empty)
ppJudgement q (id, ResParam pparams _) = 
  text "param" <+> ppIdent id <+>
  (case pparams of
     Just ps -> equals <+> fsep (intersperse (char '|') (map (ppParam q) ps))
     _       -> empty) <+> semi
ppJudgement q (id, ResValue pvalue) = empty
ppJudgement q (id, ResOper  ptype pexp) =
  text "oper" <+> ppIdent id <+>
  (case ptype of {Just (L _ t) -> colon  <+> ppTerm q 0 t; Nothing -> empty} $$
   case pexp  of {Just (L _ e) -> equals <+> ppTerm q 0 e; Nothing -> empty}) <+> semi
ppJudgement q (id, ResOverload ids defs) =
  text "oper" <+> ppIdent id <+> equals <+> 
  (text "overload" <+> lbrace $$
   nest 2 (vcat [ppIdent id <+> (colon <+> ppTerm q 0 ty $$ equals <+> ppTerm q 0 e) | (L _ ty,L _ e) <- defs]) $$
   rbrace) <+> semi
ppJudgement q (id, CncCat  ptype pexp pprn) =
  (case ptype of
     Just (L _ typ) -> text "lincat" <+> ppIdent id <+> equals <+> ppTerm q 0 typ <+> semi
     Nothing        -> empty) $$
  (case pexp of
     Just (L _ exp) -> text "lindef" <+> ppIdent id <+> equals <+> ppTerm q 0 exp <+> semi
     Nothing        -> empty) $$
  (case pprn of
     Just (L _ prn) -> text "printname" <+> text "cat" <+> ppIdent id <+> equals <+> ppTerm q 0 prn <+> semi
     Nothing        -> empty)
ppJudgement q (id, CncFun  ptype pdef pprn) =
  (case pdef of
     Just (L _ e) -> let (xs,e') = getAbs e
                     in text "lin" <+> ppIdent id <+> hsep (map ppBind xs) <+> equals <+> ppTerm q 0 e' <+> semi
     Nothing      -> empty) $$
  (case pprn of
     Just (L _ prn) -> text "printname" <+> text "fun" <+> ppIdent id <+> equals <+> ppTerm q 0 prn <+> semi
     Nothing        -> empty)
ppJudgement q (id, AnyInd cann mid) = text "ind" <+> ppIdent id <+> equals <+> (if cann then text "canonical" else empty) <+> ppIdent mid <+> semi

ppTerm q d (Abs b v e)   = let (xs,e') = getAbs (Abs b v e)
                           in prec d 0 (char '\\' <> commaPunct ppBind xs <+> text "->" <+> ppTerm q 0 e')
ppTerm q d (T TRaw xs) = case getCTable (T TRaw xs) of
                           ([],_) -> text "table" <+> lbrace $$
	  		             nest 2 (vcat (punctuate semi (map (ppCase q) xs))) $$
			             rbrace
                           (vs,e) -> prec d 0 (text "\\\\" <> commaPunct ppIdent vs <+> text "=>" <+> ppTerm q 0 e)
ppTerm q d (T (TTyped t) xs) = text "table" <+> ppTerm q 0 t <+> lbrace $$
			       nest 2 (vcat (punctuate semi (map (ppCase q) xs))) $$
			       rbrace
ppTerm q d (T (TComp  t) xs) = text "table" <+> ppTerm q 0 t <+> lbrace $$
		 	       nest 2 (vcat (punctuate semi (map (ppCase q) xs))) $$
			       rbrace
ppTerm q d (T (TWild  t) xs) = text "table" <+> ppTerm q 0 t <+> lbrace $$
			       nest 2 (vcat (punctuate semi (map (ppCase q) xs))) $$
			       rbrace
ppTerm q d (Prod bt x a b)= if x == identW && bt == Explicit
                              then prec d 0 (ppTerm q 4 a <+> text "->" <+> ppTerm q 0 b)
                              else prec d 0 (parens (ppBind (bt,x) <+> colon <+> ppTerm q 0 a) <+> text "->" <+> ppTerm q 0 b)
ppTerm q d (Table kt vt)=prec d 0 (ppTerm q 3 kt <+> text "=>" <+> ppTerm q 0 vt)
ppTerm q d (Let l e)    = let (ls,e') = getLet e
                          in prec d 0 (text "let" <+> vcat (map (ppLocDef q) (l:ls)) $$ text "in" <+> ppTerm q 0 e')
ppTerm q d (Example e s)=prec d 0 (text "in" <+> ppTerm q 5 e <+> str s)
ppTerm q d (C e1 e2)    =prec d 1 (ppTerm q 2 e1 <+> text "++" <+> ppTerm q 1 e2)
ppTerm q d (Glue e1 e2) =prec d 2 (ppTerm q 3 e1 <+> char '+'  <+> ppTerm q 2 e2)
ppTerm q d (S x y)     = case x of
                           T annot xs -> let e = case annot of
			                           TRaw     -> y
			                           TTyped t -> Typed y t
			                           TComp  t -> Typed y t
			                           TWild  t -> Typed y t
			                 in text "case" <+> ppTerm q 0 e <+> text "of" <+> lbrace $$
			                    nest 2 (vcat (punctuate semi (map (ppCase q) xs))) $$
			                   rbrace
			   _          -> prec d 3 (ppTerm q 3 x <+> text "!" <+> ppTerm q 4 y)
ppTerm q d (ExtR x y)  = prec d 3 (ppTerm q 3 x <+> text "**" <+> ppTerm q 4 y)
ppTerm q d (App x y)   = prec d 4 (ppTerm q 4 x <+> ppTerm q 5 y)
ppTerm q d (V e es)    = text "table" <+> ppTerm q 6 e <+> lbrace $$
                         nest 2 (fsep (punctuate semi (map (ppTerm q 0) es))) $$
                         rbrace
ppTerm q d (FV es)     = text "variants" <+> braces (fsep (punctuate semi (map (ppTerm q 0) es)))
ppTerm q d (Alts (e,xs))=text "pre" <+> braces (ppTerm q 0 e <> semi <+> fsep (punctuate semi (map (ppAltern q) xs)))
ppTerm q d (Strs es)   = text "strs" <+> braces (fsep (punctuate semi (map (ppTerm q 0) es)))
ppTerm q d (EPatt p)   = prec d 4 (char '#' <+> ppPatt q 2 p)
ppTerm q d (EPattType t)=prec d 4 (text "pattern" <+> ppTerm q 0 t)
ppTerm q d (P t l)     = prec d 5 (ppTerm q 5 t <> char '.' <> ppLabel l)
ppTerm q d (Cn id)     = ppIdent id
ppTerm q d (Vr id)     = ppIdent id
ppTerm q d (Q  m id)   = ppQIdent q m id
ppTerm q d (QC m id)   = ppQIdent q m id
ppTerm q d (Sort id)   = ppIdent id
ppTerm q d (K s)       = str s
ppTerm q d (EInt n)    = integer n
ppTerm q d (EFloat f)  = double f
ppTerm q d (Meta _)    = char '?'
ppTerm q d (Empty)     = text "[]"
ppTerm q d (R [])      = text "<>" -- to distinguish from {} empty RecType
ppTerm q d (R xs)      = braces (fsep (punctuate semi [ppLabel l <+> 
                                                       fsep [case mb_t of {Just t -> colon <+> ppTerm q 0 t; Nothing -> empty},
                                                             equals <+> ppTerm q 0 e] | (l,(mb_t,e)) <- xs]))
ppTerm q d (RecType xs)= braces (fsep (punctuate semi [ppLabel l <+> colon <+> ppTerm q 0 t | (l,t) <- xs]))
ppTerm q d (Typed e t) = char '<' <> ppTerm q 0 e <+> colon <+> ppTerm q 0 t <> char '>'
ppTerm q d (ImplArg e) = braces (ppTerm q 0 e)

ppEquation q (ps,e) = hcat (map (ppPatt q 2) ps) <+> text "->" <+> ppTerm q 0 e

ppCase q (p,e) = ppPatt q 0 p <+> text "=>" <+> ppTerm q 0 e

ppPatt q d (PAlt p1 p2) = prec d 0 (ppPatt q 0 p1 <+> char '|' <+> ppPatt q 1 p2)
ppPatt q d (PSeq p1 p2) = prec d 0 (ppPatt q 0 p1 <+> char '+' <+> ppPatt q 1 p2)
ppPatt q d (PC f ps)    = if null ps
                            then ppIdent f
                            else prec d 1 (ppIdent f <+> hsep (map (ppPatt q 3) ps))
ppPatt q d (PP f g ps)  = if null ps
                            then ppQIdent q f g
                            else prec d 1 (ppQIdent q f g <+> hsep (map (ppPatt q 3) ps))
ppPatt q d (PRep p)     = prec d 1 (ppPatt q 3 p <> char '*')
ppPatt q d (PAs f p)    = prec d 2 (ppIdent f <> char '@' <> ppPatt q 3 p)
ppPatt q d (PNeg p)     = prec d 2 (char '-' <> ppPatt q 3 p)
ppPatt q d (PChar)      = char '?'
ppPatt q d (PChars s)   = brackets (str s)
ppPatt q d (PMacro id)  = char '#' <> ppIdent id
ppPatt q d (PM m id)    = char '#' <> ppIdent m <> char '.' <> ppIdent id
ppPatt q d PW           = char '_'
ppPatt q d (PV id)      = ppIdent id
ppPatt q d (PInt n)     = integer n
ppPatt q d (PFloat f)   = double f
ppPatt q d (PString s)  = str s
ppPatt q d (PR xs)      = braces (hsep (punctuate semi [ppLabel l <+> equals <+> ppPatt q 0 e | (l,e) <- xs]))
ppPatt q d (PImplArg p) = braces (ppPatt q 0 p)
ppPatt q d (PTilde t)   = prec d 2 (char '~' <> ppTerm q 6 t)

ppValue :: TermPrintQual -> Int -> Val -> Doc
ppValue q d (VGen i x)    = ppIdent x <> text "{-" <> int i <> text "-}" ---- latter part for debugging
ppValue q d (VApp u v)    = prec d 4 (ppValue q 4 u <+> ppValue q 5 v)
ppValue q d (VCn (_,c))   = ppIdent c
ppValue q d (VClos env e) = case e of
                              Meta _ -> ppTerm q d e <> ppEnv env
                              _      -> ppTerm q d e ---- ++ prEnv env ---- for debugging
ppValue q d (VRecType xs) = braces (hsep (punctuate comma [ppLabel l <> char '=' <> ppValue q 0 v | (l,v) <- xs]))
ppValue q d VType         = text "Type"

ppConstrs :: Constraints -> [Doc]
ppConstrs = map (\(v,w) -> braces (ppValue Unqualified 0 v <+> text "<>" <+> ppValue Unqualified 0 w))

ppEnv :: Env -> Doc
ppEnv e = hcat (map (\(x,t) -> braces (ppIdent x <> text ":=" <> ppValue Unqualified 0 t)) e)

str s = doubleQuotes (text s)

ppDecl q (_,id,typ)
  | id == identW = ppTerm q 4 typ
  | otherwise    = parens (ppIdent id <+> colon <+> ppTerm q 0 typ)

ppDDecl q (_,id,typ)
  | id == identW = ppTerm q 6 typ
  | otherwise    = parens (ppIdent id <+> colon <+> ppTerm q 0 typ)

ppQIdent q m id =
  case q of
    Qualified   -> ppIdent m <> char '.' <> ppIdent id
    Unqualified ->                          ppIdent id

ppLabel = ppIdent . label2ident

ppOpenSpec (OSimple id)   = ppIdent id
ppOpenSpec (OQualif id n) = parens (ppIdent id <+> equals <+> ppIdent n)

ppInstSpec (id,n) = parens (ppIdent id <+> equals <+> ppIdent n)

ppLocDef q (id, (mbt, e)) =
  ppIdent id <+>
  (case mbt of {Just t -> colon  <+> ppTerm q 0 t; Nothing -> empty} <+> equals <+> ppTerm q 0 e) <+> semi

ppBind (Explicit,v) = ppIdent v
ppBind (Implicit,v) = braces (ppIdent v)

ppAltern q (x,y) = ppTerm q 0 x <+> char '/' <+> ppTerm q 0 y

ppParam q (L _ (id,cxt)) = ppIdent id <+> hsep (map (ppDDecl q) cxt)

ppPosition :: Ident -> (Int,Int) -> Doc
ppPosition m (b,e)
  | b == e    = text "in" <+> ppIdent m <> text ".gf, line"  <+> int b
  | otherwise = text "in" <+> ppIdent m <> text ".gf, lines" <+> int b <> text "-" <> int e

commaPunct f ds = (hcat (punctuate comma (map f ds)))

prec d1 d2 doc
  | d1 > d2   = parens doc
  | otherwise = doc

getAbs :: Term -> ([(BindType,Ident)], Term)
getAbs (Abs bt v e) = let (xs,e') = getAbs e
                      in ((bt,v):xs,e')
getAbs e            = ([],e)

getCTable :: Term -> ([Ident], Term)
getCTable (T TRaw [(PV v,e)]) = let (vs,e') = getCTable e
                                in (v:vs,e')
getCTable (T TRaw [(PW,  e)]) = let (vs,e') = getCTable e
                                in (identW:vs,e')
getCTable e                   = ([],e)

getLet :: Term -> ([LocalDef], Term)
getLet (Let l e) = let (ls,e') = getLet e
                   in (l:ls,e')
getLet e         = ([],e)
