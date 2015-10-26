----------------------------------------------------------------------
-- |
-- Module      : PatternMatch
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/10/12 12:38:29 $ 
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.7 $
--
-- pattern matching for both concrete and abstract syntax. AR -- 16\/6\/2003
-----------------------------------------------------------------------------

module GF.Grammar.PatternMatch (matchPattern,
		     testOvershadow, 
		     findMatch
		    ) where

import GF.Data.Operations
import GF.Grammar.Grammar
import GF.Infra.Ident
import GF.Grammar.Macros
import GF.Grammar.Printer

import Data.List
import Control.Monad
import Text.PrettyPrint
import Debug.Trace

matchPattern :: [(Patt,Term)] -> Term -> Err (Term, Substitution)
matchPattern pts term = 
  if not (isInConstantForm term)
    then Bad (render (text "variables occur in" <+> ppTerm Unqualified 0 term))
  else do
    term' <- mkK term
    errIn (render (text "trying patterns" <+> hsep (punctuate comma (map (ppPatt Unqualified 0 . fst) pts)))) $
      findMatch [([p],t) | (p,t) <- pts] [term']
 where
  -- to capture all Str with string pattern matching
  mkK s = case s of
    C _ _ -> do
      s' <- getS s
      return (K (unwords s'))
    _ -> return s

  getS s = case s of
    K w -> return [w]
    C v w -> liftM2 (++) (getS v) (getS w)
    Empty -> return []
    _ -> Bad (render (text "cannot get string from" <+> ppTerm Unqualified 0 s))

testOvershadow :: [Patt] -> [Term] -> Err [Patt]
testOvershadow pts vs = do
  let numpts = zip pts [0..]
  let cases  = [(p,EInt i) | (p,i) <- numpts]
  ts <- mapM (liftM fst . matchPattern cases) vs
  return [p | (p,i) <- numpts, notElem i [i | EInt i <- ts] ]

findMatch :: [([Patt],Term)] -> [Term] -> Err (Term, Substitution)
findMatch cases terms = case cases of
   [] -> Bad (render (text "no applicable case for" <+> hsep (punctuate comma (map (ppTerm Unqualified 0) terms))))
   (patts,_):_ | length patts /= length terms -> 
       Bad (render (text "wrong number of args for patterns :" <+> hsep (map (ppPatt Unqualified 0) patts) <+> 
                    text "cannot take" <+> hsep (map (ppTerm Unqualified 0) terms)))
   (patts,val):cc -> case mapM tryMatch (zip patts terms) of
       Ok substs -> return (val, concat substs)
       _         -> findMatch cc terms

tryMatch :: (Patt, Term) -> Err [(Ident, Term)]
tryMatch (p,t) = do 
  t' <- termForm t
  trym p t'
 where

  isInConstantFormt = True -- tested already in matchPattern
  trym p t' =
    case (p,t') of
      (_,(x,Empty,y)) -> trym p (x,K [],y)   -- because "" = [""] = []
      (PW, _) | isInConstantFormt -> return [] -- optimization with wildcard
      (PV x,  _) | isInConstantFormt -> return [(x,t)]
      (PString s, ([],K i,[])) | s==i -> return []
      (PInt s, ([],EInt i,[])) | s==i -> return []
      (PFloat s,([],EFloat i,[])) | s==i -> return [] --- rounding?
      (PC p pp, ([], Con f, tt)) | 
            p `eqStrIdent` f && length pp == length tt ->
         do matches <- mapM tryMatch (zip pp tt)
            return (concat matches)

      (PP q p pp, ([], QC r f, tt)) | 
            -- q `eqStrIdent` r &&  --- not for inherited AR 10/10/2005
            p `eqStrIdent` f && length pp == length tt ->
         do matches <- mapM tryMatch (zip pp tt)
            return (concat matches)
      ---- hack for AppPredef bug
      (PP q p pp, ([], Q r f, tt)) | 
            -- q `eqStrIdent` r && --- 
            p `eqStrIdent` f && length pp == length tt ->
         do matches <- mapM tryMatch (zip pp tt)
            return (concat matches)

      (PR r, ([],R r',[])) |
            all (`elem` map fst r') (map fst r) ->
         do matches <- mapM tryMatch 
                            [(p,snd a) | (l,p) <- r, let Just a = lookup l r']
            return (concat matches)
      (PT _ p',_) -> trym p' t'

      (PAs x p',_) -> do
         subst <- trym p' t'
         return $ (x,t) : subst

      (PAlt p1 p2,_) -> checks [trym p1 t', trym p2 t']

      (PNeg p',_) -> case tryMatch (p',t) of
        Bad _ -> return []
        _ -> Bad (render (text "no match with negative pattern" <+> ppPatt Unqualified 0 p))

      (PSeq p1 p2, ([],K s, [])) -> do
         let cuts = [splitAt n s | n <- [0 .. length s]] 
         matches <- checks [mapM tryMatch [(p1,K s1),(p2,K s2)] | (s1,s2) <- cuts]
         return (concat matches)

      (PRep p1, ([],K s, [])) -> checks [
         trym (foldr (const (PSeq p1)) (PString "") 
           [1..n]) t' | n <- [0 .. length s]
        ] >>
        return []

      (PChar,  ([],K [_], [])) -> return []
      (PChars cs, ([],K [c], [])) | elem c cs -> return []

      _ -> Bad (render (text "no match in case expr for" <+> ppTerm Unqualified 0 t))
  
isInConstantForm :: Term -> Bool
isInConstantForm trm = case trm of
    Cn _     -> True
    Con _    -> True
    Q _ _    -> True
    QC _ _   -> True
    Abs _ _ _ -> True
    C c a    -> isInConstantForm c && isInConstantForm a
    App c a  -> isInConstantForm c && isInConstantForm a
    R r      -> all (isInConstantForm . snd . snd) r
    K _      -> True
    Empty    -> True
    EInt _   -> True
    _       -> False ---- isInArgVarForm trm

varsOfPatt :: Patt -> [Ident]
varsOfPatt p = case p of
  PV x -> [x]
  PC _ ps -> concat $ map varsOfPatt ps
  PP _ _ ps -> concat $ map varsOfPatt ps
  PR r    -> concat $ map (varsOfPatt . snd) r
  PT _ q -> varsOfPatt q
  _ -> []

-- | to search matching parameter combinations in tables
isMatchingForms :: [Patt] -> [Term] -> Bool
isMatchingForms ps ts = all match (zip ps ts') where
  match (PC c cs, (Cn d, ds)) = c == d && isMatchingForms cs ds
  match _ = True
  ts' = map appForm ts

