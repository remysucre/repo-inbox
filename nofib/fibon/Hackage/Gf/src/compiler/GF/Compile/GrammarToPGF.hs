{-# LANGUAGE PatternGuards #-}
module GF.Compile.GrammarToPGF (mkCanon2pgf) where

import GF.Compile.Export
import GF.Compile.GeneratePMCFG

import PGF.CId
import PGF.Macros(updateProductionIndices)
import PGF.Check(checkLin)
import qualified PGF.Macros as CM
import qualified PGF.Data as C
import qualified PGF.Data as D
import GF.Grammar.Predef
import GF.Grammar.Printer
import GF.Grammar.Grammar
import qualified GF.Grammar.Lookup as Look
import qualified GF.Grammar as A
import qualified GF.Grammar.Macros as GM
import qualified GF.Compile.Concrete.Compute as Compute ---- 
import qualified GF.Infra.Modules as M
import qualified GF.Infra.Option as O

import GF.Infra.Ident
import GF.Infra.Option
import GF.Data.Operations

import Data.List
import Data.Function
import Data.Char (isDigit,isSpace)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Text.PrettyPrint
import Debug.Trace ----

-- when developing, swap commenting
--traceD s t = trace s t 
traceD s t = t 


-- the main function: generate PGF from GF.
mkCanon2pgf :: Options -> String -> SourceGrammar -> IO D.PGF
mkCanon2pgf opts cnc gr = (canon2pgf opts pars . reorder abs . canon2canon opts abs) gr
  where
    abs = err (const c) id $ M.abstractOfConcrete gr c where c = identC (BS.pack cnc)
    pars = mkParamLincat gr

-- Generate PGF from GFCM.
-- this assumes a grammar translated by canon2canon

canon2pgf :: Options -> (Ident -> Ident -> C.Term) -> SourceGrammar -> IO D.PGF
canon2pgf opts pars cgr@(M.MGrammar ((a,abm):cms)) = do
  if dump opts DumpCanon
    then putStrLn (render (vcat (map (ppModule Qualified) (M.modules cgr))))
    else return ()
  cncs <- sequence [mkConcr lang (i2i lang) mo | (lang,mo) <- cms]
  return $ updateProductionIndices (D.PGF gflags an abs (Map.fromList cncs))
 where
  -- abstract
  an  = (i2i a)
  abs = D.Abstr aflags funs cats
  gflags = Map.empty
  aflags = Map.fromList [(mkCId f,C.LStr x) | (f,x) <- optionsPGF (M.flags abm)]

  mkDef (Just eqs) = Just [C.Equ ps' (mkExp scope' e) | L _ (ps,e) <- eqs, let (scope',ps') = mapAccumL mkPatt [] ps]
  mkDef Nothing    = Nothing

  mkArrity (Just a) = a
  mkArrity Nothing  = 0

  -- concretes
  lfuns = [(f', (mkType [] ty, mkArrity ma, mkDef pty)) | 
             (f,AbsFun (Just (L _ ty)) ma pty) <- tree2list (M.jments abm), let f' = i2i f]
  funs = Map.fromAscList lfuns
  lcats = [(i2i c, (snd (mkContext [] cont),catfuns c)) |
                (c,AbsCat (Just (L _ cont))) <- tree2list (M.jments abm)]
  cats = Map.fromAscList lcats
  catfuns cat =
    (map snd . sortBy (compare `on` fst))
        [(loc,i2i f) | (f,AbsFun (Just (L loc ty)) _ _) <- tree2list (M.jments abm), snd (GM.valCat ty) == cat]

  mkConcr lang0 lang mo = do
    lins' <- case mapM (checkLin (funs,lins,lincats) lang) (Map.toList lins) of
               Ok x    -> return x
               Bad msg -> fail msg
    cnc <- convertConcrete opts lang flags printnames funs (Map.fromList (map fst lins')) lincats params lindefs
    return (lang, cnc)
    where
      js = tree2list (M.jments mo)
      flags   = Map.fromList [(mkCId f,C.LStr x) | (f,x) <- optionsPGF (M.flags mo)]
      utf     = id -- trace (show lang0 +++ show flags) $ 
                -- if moduleFlag optEncoding (moduleOptions (M.flags mo)) == UTF_8
                --  then id else id 
                ---- then (trace "decode" D.convertStringsInTerm decodeUTF8) else id
      umkTerm = utf . mkTerm
      lins    = Map.fromAscList 
        [(f', umkTerm tr)  | (f,CncFun _ (Just (L _ tr)) _) <- js, 
            let f' = i2i f, exists f'] -- eliminating lins without fun 
            -- needed even here because of restricted inheritance
      lincats = Map.fromAscList 
        [(i2i c, mkCType ty) | (c,CncCat (Just (L _ ty)) _ _) <- js]
      lindefs = Map.fromAscList 
        [(i2i c, umkTerm tr)  | (c,CncCat _ (Just (L _ tr)) _) <- js]
      printnames = Map.union 
        (Map.fromAscList [(i2i f, realize (umkTerm tr)) | (f,CncFun _ _ (Just (L _ tr))) <- js])
        (Map.fromAscList [(i2i f, realize (umkTerm tr)) | (f,CncCat _ _ (Just (L _ tr))) <- js])
      params = Map.fromAscList 
        [(i2i c, pars lang0 c) | (c,CncCat (Just ty) _ _) <- js]
      fcfg = Nothing

      exists f = Map.member f funs

i2i :: Ident -> CId
i2i = CId . ident2bs

b2b :: A.BindType -> C.BindType
b2b A.Explicit = C.Explicit
b2b A.Implicit = C.Implicit

mkType :: [Ident] -> A.Type -> C.Type
mkType scope t =
  case GM.typeForm t of
    (hyps,(_,cat),args) -> let (scope',hyps') = mkContext scope hyps
                           in C.DTyp hyps' (i2i cat) (map (mkExp scope') args)

mkExp :: [Ident] -> A.Term -> C.Expr
mkExp scope t = 
  case t of
    Q _ c    -> C.EFun (i2i c)
    QC _ c   -> C.EFun (i2i c)
    Vr x     -> case lookup x (zip scope [0..]) of
                  Just i  -> C.EVar  i
                  Nothing -> C.EMeta 0
    Abs b x t-> C.EAbs (b2b b) (i2i x) (mkExp (x:scope) t)
    App t1 t2-> C.EApp (mkExp scope t1) (mkExp scope t2)
    EInt i   -> C.ELit (C.LInt (fromIntegral i))
    EFloat f -> C.ELit (C.LFlt f)
    K s      -> C.ELit (C.LStr s)
    Meta i   -> C.EMeta i
    _        -> C.EMeta 0

mkPatt scope p = 
  case p of
    A.PP _ c ps -> let (scope',ps') = mapAccumL mkPatt scope ps
                   in (scope',C.PApp (i2i c) ps')
    A.PV x      -> (x:scope,C.PVar (i2i x))
    A.PAs x p   -> let (scope',p') = mkPatt scope p
                   in (x:scope',C.PAs (i2i x) p')
    A.PW        -> (  scope,C.PWild)
    A.PInt i    -> (  scope,C.PLit (C.LInt (fromIntegral i)))
    A.PFloat f  -> (  scope,C.PLit (C.LFlt f))
    A.PString s -> (  scope,C.PLit (C.LStr s))
    A.PImplArg p-> let (scope',p') = mkPatt scope p
                   in (scope',C.PImplArg p')
    A.PTilde t  -> (  scope,C.PTilde (mkExp scope t))


mkContext :: [Ident] -> A.Context -> ([Ident],[C.Hypo])
mkContext scope hyps = mapAccumL (\scope (bt,x,ty) -> let ty' = mkType scope ty
                                                      in if x == identW
                                                           then (  scope,(b2b bt,i2i x,ty'))
                                                           else (x:scope,(b2b bt,i2i x,ty'))) scope hyps 

mkTerm :: Term -> C.Term
mkTerm tr = case tr of
  Vr (IA _ i) -> C.V i
  Vr (IAV _ _ i) -> C.V i
  Vr (IC s) | isDigit (BS.last s) -> 
    C.V ((read . BS.unpack . snd . BS.spanEnd isDigit) s)
    ---- from gf parser of gfc
  EInt i      -> C.C $ fromInteger i
  R rs     -> C.R [mkTerm t | (_, (_,t)) <- rs]
  P t l    -> C.P (mkTerm t) (C.C (mkLab l))
  T _ cs   -> C.R [mkTerm t | (_,t) <- cs] ------
  V _ cs   -> C.R [mkTerm t | t <- cs]
  S t p    -> C.P (mkTerm t) (mkTerm p)
  C s t    -> C.S $ concatMap flats [mkTerm x | x <- [s,t]]
  FV ts    -> C.FV [mkTerm t | t <- ts]
  K s      -> C.K (C.KS s)
-----  K (KP ss _) -> C.K (C.KP ss []) ---- TODO: prefix variants
  Empty    -> C.S []
  App _ _  -> prtTrace tr $ C.C 66661          ---- for debugging
  Abs _ _ t -> mkTerm t ---- only on toplevel
  Alts (td,tvs) -> 
    C.K (C.KP (strings td) [C.Alt (strings u) (strings v) | (u,v) <- tvs])
  _ -> prtTrace tr $ C.S [C.K (C.KS (render (A.ppTerm Unqualified 0 tr <+> int 66662)))] ---- for debugging
 where
   mkLab (LIdent l) = case BS.unpack l of
     '_':ds -> (read ds) :: Int
     _ -> prtTrace tr $ 66663
   strings t = case t of
     K s -> [s]
     C u v -> strings u ++ strings v
     Strs ss -> concatMap strings ss
     _ -> prtTrace tr $ ["66660"]
   flats t = case t of
     C.S ts -> concatMap flats ts
     _ -> [t]

-- encoding PGF-internal lincats as terms
mkCType :: Type -> C.Term
mkCType t = case t of
  EInt i      -> C.C $ fromInteger i
  RecType rs  -> C.R [mkCType t | (_, t) <- rs]
  Table pt vt -> case pt of
    EInt i -> C.R $ replicate (1 + fromInteger i) $ mkCType vt
    RecType rs -> mkCType $ foldr Table vt (map snd rs) 
    _ | Just i <- GM.isTypeInts pt -> C.R $ replicate (fromInteger i) $ mkCType vt

  Sort s | s == cStr -> C.S [] --- Str only
  _ | Just i <- GM.isTypeInts t -> C.C $ fromInteger i
  _ -> error  $ "mkCType " ++ show t

-- encoding showable lincats (as in source gf) as terms
mkParamLincat :: SourceGrammar -> Ident -> Ident -> C.Term
mkParamLincat sgr lang cat = errVal (C.R [C.S []]) $ do 
  typ <- Look.lookupLincat sgr lang cat
  mkPType typ
 where
  mkPType typ = case typ of
    RecType lts -> do
      ts <- mapM (mkPType . snd) lts
      return $ C.R [ C.P (kks $ showIdent (label2ident l)) t | ((l,_),t) <- zip lts ts]
    Table (RecType lts) v -> do
      ps <- mapM (mkPType . snd) lts
      v' <- mkPType v
      return $ foldr (\p v -> C.S [p,v]) v' ps
    Table p v -> do
      p' <- mkPType p
      v' <- mkPType v
      return $ C.S [p',v']
    Sort s | s == cStr -> return $ C.S []
    _ -> return $ 
      C.FV $ map (kks . renderStyle style{mode=OneLineMode} . ppTerm Unqualified 6) $ 
             errVal [] $ Look.allParamValues sgr typ
  kks = C.K . C.KS

-- return just one module per language

reorder :: Ident -> SourceGrammar -> SourceGrammar
reorder abs cg = M.MGrammar $ 
    (abs, M.ModInfo M.MTAbstract M.MSComplete aflags [] Nothing [] [] adefs):
      [(c, M.ModInfo (M.MTConcrete abs) M.MSComplete fs [] Nothing [] [] (sorted2tree js))
            | (c,(fs,js)) <- cncs] 
     where
       mos = M.modules cg
       adefs = sorted2tree $ sortIds $
                 predefADefs ++ Look.allOrigInfos cg abs
       predefADefs = 
         [(c, AbsCat (Just (L (0,0) []))) | c <- [cFloat,cInt,cString]]
       aflags = 
         concatOptions [M.flags mo | (_,mo) <- M.modules cg, M.isModAbs mo]

       cncs = sortIds [(lang, concr lang) | lang <- M.allConcretes cg abs]
       concr la = (flags, 
                   sortIds (predefCDefs ++ jments)) where 
         jments = Look.allOrigInfos cg la
         flags  = concatOptions 
                    [M.flags mo | 
                     (i,mo) <- mos, M.isModCnc mo, 
                     Just r <- [lookup i (M.allExtendSpecs cg la)]]

         predefCDefs = 
           [(c, CncCat (Just (L (0,0) GM.defLinType)) Nothing Nothing) | c <- [cInt,cFloat,cString]]

       sortIds = sortBy (\ (f,_) (g,_) -> compare f g) 


-- one grammar per language - needed for symtab generation
repartition :: Ident -> SourceGrammar -> [SourceGrammar]
repartition abs cg = 
  [M.partOfGrammar cg (lang,mo) | 
    let mos = M.modules cg,
    lang <- case M.allConcretes cg abs of
      [] -> [abs]  -- to make pgf nonempty even when there are no concretes
      cncs -> cncs,
    let mo = errVal 
         (error (render (text "no module found for" <+> A.ppIdent lang))) $ M.lookupModule cg lang
    ]

-- translate tables and records to arrays, parameters and labels to indices

canon2canon :: Options -> Ident -> SourceGrammar -> SourceGrammar
canon2canon opts abs cg0 = 
   (recollect . map cl2cl . repartition abs . purgeGrammar abs) cg0 
 where
  recollect = M.MGrammar . nubBy (\ (i,_) (j,_) -> i==j) . concatMap M.modules
  cl2cl = M.MGrammar . js2js . map (c2c p2p) . M.modules
    
  js2js ms = map (c2c (j2j (M.MGrammar ms))) ms

  c2c f2 (c,mo) = (c, M.replaceJudgements mo $ mapTree f2 (M.jments mo))

  j2j cg (f,j) = 
    let debug = if verbAtLeast opts Verbose then trace ("+ " ++ showIdent f) else id in
    case j of
      CncFun x (Just (L loc tr)) z -> CncFun x (Just (L loc (debug (t2t (unfactor cg0 tr))))) z
      CncCat (Just (L locty ty)) (Just (L locx x)) y -> CncCat (Just (L locty (ty2ty ty))) (Just (L locx (t2t (unfactor cg0 x)))) y
      _ -> j
   where
      cg1 = cg
      t2t = term2term f cg1 pv
      ty2ty = type2type cg1 pv
      pv@(labels,untyps,typs) = trs $ paramValues cg1

      unfactor :: SourceGrammar -> Term -> Term
      unfactor gr t = case t of
        T (TTyped ty) [(PV x,u)] -> V ty [restore x v (unfac u) | v <- vals ty]
        _ -> GM.composSafeOp unfac t
       where
         unfac = unfactor gr
         vals  = err error id . Look.allParamValues gr
         restore x u t = case t of
           Vr y | y == x -> u
           _ -> GM.composSafeOp (restore x u) t

    -- flatten record arguments of param constructors
  p2p (f,j) = case j of
      ResParam (Just ps) (Just vs) -> 
        ResParam (Just [L loc (c,concatMap unRec cont) | L loc (c,cont) <- ps]) (Just (map unrec vs))
      _ -> j
  unRec (bt,x,ty) = case ty of
      RecType fs -> [ity | (_,typ) <- fs, ity <- unRec (Explicit,identW,typ)] 
      _ -> [(bt,x,ty)]
  unrec t = case t of
     App f (R fs) -> GM.mkApp (unrec f) [unrec u | (_,(_,u)) <- fs]
     _ -> GM.composSafeOp unrec t


----
  trs v = traceD (render (tr v)) v

  tr (labels,untyps,typs) =
     (text "LABELS:" <+>
        vcat [A.ppIdent c <> char '.' <> hsep (map A.ppLabel l) <+> char '=' <+> text (show i)  | ((c,l),i) <- Map.toList labels]) $$
     (text "UNTYPS:" <+>
        vcat [A.ppTerm Unqualified 0 t <+> char '=' <+> text (show i) | (t,i) <- Map.toList untyps]) $$
     (text "TYPS:  " <+>
        vcat [A.ppTerm Unqualified 0 t <+> char '=' <+> text (show (Map.assocs i)) | (t,i) <- Map.toList typs])
----

purgeGrammar :: Ident -> SourceGrammar -> SourceGrammar
purgeGrammar abstr gr = 
  (M.MGrammar . list . filter complete . purge . M.modules) gr 
 where
  list ms = traceD (render (text "MODULES" <+> hsep (punctuate comma (map (ppIdent . fst) ms)))) ms
  purge = nubBy (\x y -> fst x == fst y) . filter (flip elem needed . fst)
  needed = nub $ concatMap (requiredCanModules isSingle gr) acncs
  acncs = abstr : M.allConcretes gr abstr
  isSingle = True
  complete (i,m) = M.isCompleteModule m --- not . isIncompleteCanon

type ParamEnv =
  (Map.Map (Ident,[Label]) (Type,Integer), -- numbered labels
   Map.Map Term Integer,                   -- untyped terms to values
   Map.Map Type (Map.Map Term Integer))    -- types to their terms to values

--- gathers those param types that are actually used in lincats and lin terms
paramValues :: SourceGrammar -> ParamEnv
paramValues cgr = (labels,untyps,typs) where
  partyps = nub $ 
            --- [App (Q (IC "Predef") (IC "Ints")) (EInt i) | i <- [1,9]] ---linTypeInt 
            [ty | 
              (_,(_,CncCat (Just (L _ ty0)) _ _)) <- jments,
              ty  <- typsFrom ty0
            ] ++ [
             Q m ty | 
              (m,(ty,ResParam _ _)) <- jments
            ] ++ [ty | 
              (_,(_,CncFun _ (Just (L _ tr)) _)) <- jments,
              ty  <- err (const []) snd $ appSTM (typsFromTrm tr) []
            ]
  params = [(ty, errVal (traceD ("UNKNOWN PARAM TYPE" +++ show ty) []) $ 
                                         Look.allParamValues cgr ty) | ty <- partyps]
  typsFrom ty = (if isParam ty then (ty:) else id) $ case ty of
    Table p t  -> typsFrom p ++ typsFrom t
    RecType ls -> concat [typsFrom t | (_, t) <- ls]
    _ -> []
 
  isParam ty = case ty of
    Q _ _ -> True
    QC _ _ -> True
    RecType rs -> all isParam (map snd rs)
    _ -> False

  typsFromTrm :: Term -> STM [Type] Term
  typsFromTrm tr = case tr of
    R fs -> mapM_ (typsFromField . snd) fs >> return tr 
      where
        typsFromField (mty, t) = case mty of
          Just x -> updateSTM (x:) >> typsFromTrm t
          _ -> typsFromTrm t
    V ty ts -> updateSTM (ty:) >> mapM_ typsFromTrm ts >> return tr
    T (TTyped ty) cs -> 
      updateSTM (ty:) >> mapM_ typsFromTrm [t | (_, t) <- cs] >> return tr
    T (TComp ty) cs -> 
      updateSTM (ty:) >> mapM_ typsFromTrm [t | (_, t) <- cs] >> return tr
    _ -> GM.composOp typsFromTrm tr

  mods = traceD (render (hsep (map (ppIdent . fst) ms))) ms where ms = M.modules cgr

  jments = 
    [(m,j) | (m,mo) <- mods, j <- tree2list $ M.jments mo]
  typs = 
    Map.fromList [(ci,Map.fromList (zip vs [0..])) | (ci,vs) <- params]
  untyps = 
    Map.fromList $ concatMap Map.toList [typ | (_,typ) <- Map.toList typs]
  lincats = 
    [(cat,[f | let RecType fs = GM.defLinType, f <- fs]) | cat <- [cInt,cFloat, cString]] ++
    reverse ---- TODO: really those lincats that are reached
            ---- reverse is enough to expel overshadowed ones... 
      [(cat,ls) | (_,(cat,CncCat (Just (L _ ty)) _ _)) <- jments, 
                  RecType ls <- [unlockTy ty]]
  labels = Map.fromList $ concat 
    [((cat,[lab]),(typ,i)): 
      [((cat,[LVar v]),(typ,toInteger (mx + v))) | v <- [0,1]] ++ ---- 1 or 2 vars 
      [((cat,[lab,lab2]),(ty,j)) | 
        rs <- getRec typ, ((lab2, ty),j) <- zip rs [0..]] 
      ++
      ---- one more level, but: ...
      [((cat,[lab,lab2,lab3]),(ty,j)) | 
        rss <- getRec typ, ((lab2, ty0),j0) <- zip rss [0..],
        (_,ty2) <- rss,
        rs  <- getRec ty2, ((lab3, ty),j) <- zip rs [0..]] 
      | 
        (cat,ls) <- lincats, ((lab, typ),i) <- zip ls [0..], let mx = length ls]
    -- go to tables recursively
    ---- ... TODO: go to deeper records
   where
     getRec typ = case typ of
       RecType rs -> [rs] ---- [unlockTyp rs]  -- (sort (unlockTyp ls))
       Table _ t  -> getRec t
       _ -> []

type2type :: SourceGrammar -> ParamEnv -> Type -> Type
type2type cgr env@(labels,untyps,typs) ty = case ty of
  RecType rs ->
    RecType [(mkLab i, t2t t) | (i,(l, t)) <- zip [0..] (unlockTyp rs)]
  Table pt vt -> Table (t2t pt) (t2t vt)
  QC _ _ -> look ty
  _ -> ty
 where
   t2t = type2type cgr env
   look ty = EInt $ (+ (-1)) $ toInteger $ case Map.lookup ty typs of
     Just vs -> length $ Map.assocs vs
     _ -> trace ("unknown partype " ++ show ty) 66669

term2term :: Ident -> SourceGrammar -> ParamEnv -> Term -> Term
term2term fun cgr env@(labels,untyps,typs) tr = case tr of
  App _ _ -> mkValCase (unrec tr)
  QC  _ _ -> mkValCase tr 
  R rs    -> R [(mkLab i, (Nothing, t2t t)) | 
                 (i,(l,(_,t))) <- zip [0..] (GM.sortRec (unlock rs))]
  P  t l   -> r2r tr

  T (TWild _) _ -> error $ (render (text "wild" <+> ppTerm Qualified 0 tr))
  T (TComp  ty) cs  -> t2t $ V ty $ map snd cs ---- should be elim'ed in tc
  T (TTyped ty) cs  -> t2t $ V ty $ map snd cs ---- should be elim'ed in tc
  V ty ts  -> mkCurry $ V ty [t2t t | t <- ts]
  S t p    -> mkCurrySel (t2t t) (t2t p)

  _ -> GM.composSafeOp t2t tr
 where
   t2t = term2term fun cgr env

   unrec t = case t of
     App f (R fs) -> GM.mkApp (unrec f) [unrec u | (_,(_,u)) <- fs]
     _ -> GM.composSafeOp unrec t

   mkValCase tr = case appSTM (doVar tr) [] of
     Ok (tr', st@(_:_)) -> t2t $ comp $ foldr mkCase tr' st
     _ -> valNum $ comp tr

   --- this is mainly needed for parameter record projections
   ---- was: 
   comp t = errVal t $ Compute.computeConcreteRec cgr t

   doVar :: Term -> STM [((Type,[Term]),(Term,Term))] Term
   doVar tr = case getLab tr of
     Ok (cat, lab) -> do
       k <- readSTM >>= return . length
       let tr' = Vr $ identC $ (BS.pack (show k)) -----

       let tyvs = case Map.lookup (cat,lab) labels of
             Just (ty,_) -> case Map.lookup ty typs of
               Just vs -> (ty,[t | 
                            (t,_) <- sortBy (\x y -> compare (snd x) (snd y)) 
                                            (Map.assocs vs)])
               _ -> error $ render (text "doVar1" <+> A.ppTerm Unqualified 0 ty)
             _ -> error $ render (text "doVar2" <+> A.ppTerm Unqualified 0 tr <+> text (show (cat,lab))) ---- debug
       updateSTM ((tyvs, (tr', tr)):) 
       return tr'
     _ -> GM.composOp doVar tr

   r2r tr@(P (S (V ty ts) v) l) = t2t $ S (V ty [comp (P t l) | t <- ts]) v

   r2r tr@(P p _) = case getLab tr of
     Ok (cat,labs) -> P (t2t p) . mkLab $ 
          maybe (prtTrace tr $ 66664) snd $ 
            Map.lookup (cat,labs) labels
     _ -> K (render (A.ppTerm Unqualified 0 tr <+> prtTrace tr (int 66665)))

   -- this goes recursively into tables (ignored) and records (accumulated)
   getLab tr = case tr of
     Vr (IA cat _) -> return (identC cat,[])
     Vr (IAV cat _ _) -> return (identC cat,[])
     Vr (IC s) -> return (identC cat,[]) where
       cat = BS.takeWhile (/='_') s ---- also to match IAVs; no _ in a cat tolerated
             ---- init (reverse (dropWhile (/='_') (reverse s))) ---- from gf parser
----     Vr _ -> error $ "getLab " ++ show tr
     P p lab2 -> do
       (cat,labs) <- getLab p
       return (cat,labs++[lab2]) 
     S p _ -> getLab p
     _ -> Bad "getLab"


   mkCase ((ty,vs),(x,p)) tr = 
     S (V ty [mkBranch x v tr | v <- vs]) p
   mkBranch x t tr = case tr of
     _ | tr == x -> t
     _ -> GM.composSafeOp (mkBranch x t) tr     

   valNum tr = maybe (valNumFV $ tryFV tr) EInt $ Map.lookup tr untyps
    where
      tryFV tr = case GM.appForm tr of
        (c@(QC _ _), ts) -> [GM.mkApp c ts' | ts' <- combinations (map tryFV ts)]
        (FV ts,_) -> ts
        _ -> [tr]
      valNumFV ts = case ts of
        [tr] -> let msg = render (text "DEBUG" <+> ppIdent fun <> text ": error in valNum" <+> ppTerm Qualified 0 tr) in 
                trace msg $ error (showIdent fun)
        _ -> FV $ map valNum ts

   mkCurry trm = case trm of
     V (RecType [(_,ty)]) ts -> V ty ts 
     V (RecType ((_,ty):ltys)) ts -> 
       V ty [mkCurry (V (RecType ltys) cs) | 
         cs <- chop (product (map (lengthtyp . snd) ltys)) ts] 
     _ -> trm
   lengthtyp ty = case Map.lookup ty typs of
     Just m -> length (Map.assocs m)
     _ -> error $ "length of type " ++ show ty
   chop i xs = case splitAt i xs of 
     (xs1,[])  -> [xs1]
     (xs1,xs2) -> xs1:chop i xs2


   mkCurrySel t p = S t p -- done properly in CheckGFCC


mkLab k = LIdent (BS.pack ("_" ++ show k))

-- remove lock fields; in fact, any empty records and record types
unlock = filter notlock where
  notlock (l,(_, t)) = case t of --- need not look at l
     R [] -> False
     RecType [] -> False
     _ -> True

unlockTyp = filter notlock
  
notlock (l, t) = case t of --- need not look at l
     RecType [] -> False
     _ -> True

unlockTy ty = case ty of
  RecType ls -> RecType $ GM.sortRec [(l, unlockTy t) | (l,t) <- ls, notlock (l,t)]
  _ -> GM.composSafeOp unlockTy ty


prtTrace tr n = 
  trace (render (text "-- INTERNAL COMPILER ERROR" <+> A.ppTerm Unqualified 0 tr $$ text (show n))) n
prTrace  tr n = trace (render (text "-- OBSERVE" <+> A.ppTerm Unqualified 0 tr <+> text (show n) <+> text (show tr))) n


-- | this function finds out what modules are really needed in the canonical gr.
-- its argument is typically a concrete module name
requiredCanModules :: Bool -> M.MGrammar a -> Ident -> [Ident]
requiredCanModules isSingle gr c = nub $ filter notReuse ops ++ exts where
  exts = M.allExtends gr c
  ops  = if isSingle 
         then map fst (M.modules gr) 
         else iterFix (concatMap more) $ exts
  more i = errVal [] $ do
    m <- M.lookupModule gr i
    return $ M.extends m ++ [o | o <- map M.openedModule (M.opens m)]
  notReuse i = errVal True $ do
    m <- M.lookupModule gr i
    return $ M.isModRes m -- to exclude reused Cnc and Abs from required


realize :: C.Term -> String
realize = concat . take 1 . realizes

realizes :: C.Term -> [String]
realizes = map (unwords . untokn) . realizest

realizest :: C.Term -> [[C.Tokn]]
realizest trm = case trm of
  C.R ts     -> realizest (ts !! 0)
  C.S ss     -> map concat $ combinations $ map realizest ss
  C.K t      -> [[t]]
  C.W s t    -> [[C.KS (s ++ r)] | [C.KS r] <- realizest t]
  C.FV ts    -> concatMap realizest ts
  C.TM s     -> [[C.KS s]]
  _ -> [[C.KS $ "REALIZE_ERROR " ++ show trm]] ---- debug

untokn :: [C.Tokn] -> [String]
untokn ts = case ts of
  C.KP d _  : [] -> d
  C.KP d vs : ws -> let ss@(s:_) = untokn ws in sel d vs s ++ ss
  C.KS s    : ws -> s : untokn ws
  []             -> []
 where
   sel d vs w = case [v | C.Alt v cs <- vs, any (\c -> isPrefixOf c w) cs] of
     v:_ -> v
     _   -> d
