-- Protocol data structures

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Protocol (Event (..), evtTerm, evtMap, Trace, evt,
    tterms, originates, originationPos, acquiredPos, usedPos, Role,
    rname, rvars, rtrace, rnon, runique, rcomment, rnorig, ruorig,
    mkRole, varSubset, varsInTerms, addVars, Prot, mkProt, pname, alg,
    pgen, roles, pcomment, flow) where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Set (Set)
import CPSA.Lib.Utilities
import CPSA.Lib.SExpr
import CPSA.Lib.Algebra

-- Useful operations on variables

-- Are the vars in ts a subset of the ones in ts'?
varSubset :: Algebra t p g s e c => [t] -> [t] -> Bool
varSubset ts ts' =
    all (flip elem (varsInTerms ts')) (varsInTerms ts)

varsInTerms :: Algebra t p g s e c => [t] -> [t]
varsInTerms ts =
    foldl addVars [] ts

addVars :: Algebra t p g s e c => [t] -> t -> [t]
addVars ts t = foldVars (flip adjoin) ts t

-- Message events and traces

data Event t p g s e c
    = In !t                      -- Inbound message
    | Out !t                     -- Outbound messasge
      deriving (Show, Eq, Ord)

-- Dispatch to function based on direction.
evt :: Algebra t p g s e c => (t -> a) -> (t -> a) ->
       Event t p g s e c -> a
evt inDir outDir evt =
    case evt of
      In t -> inDir t
      Out t -> outDir t

-- Extract the term in a directed term (evt id id).
evtTerm :: Algebra t p g s e c => Event t p g s e c -> t
evtTerm (In t) = t
evtTerm (Out t) = t

-- Map the term in a directed term.
evtMap :: Algebra t p g s e c => (t -> t) ->
          Event t p g s e c -> Event t p g s e c
evtMap f (In t) = In (f t)
evtMap f (Out t) = Out (f t)

-- A trace is a list of directed terms.  The terms in the trace are
-- stored in causal order.
type Trace t p g s e c = [Event t p g s e c]

-- The set of terms in a trace.
tterms :: Algebra t p g s e c => Trace t p g s e c -> [t]
tterms c =
    foldl (\ts evt -> adjoin (evtTerm evt) ts) [] c

-- Is the term carried by a directed term, and is the first one outgoing?
originates :: Algebra t p g s e c => t -> Trace t p g s e c -> Bool
originates _ [] = False         -- Term is not carried
originates t (Out t' : c) = t `carriedBy` t' || originates t c
originates t (In t' : c) = not (t `carriedBy` t') && originates t c

-- At what position does a term originate in a trace?
originationPos :: Algebra t p g s e c => t ->
                  Trace t p g s e c -> Maybe Int
originationPos t c =
    loop 0 c
    where
      loop _ [] = Nothing       -- Term is not carried
      loop pos (Out t' : c)
          | t `carriedBy` t' = Just pos -- Found it
          | otherwise = loop (pos + 1) c
      loop pos (In t' : c)
          | t `carriedBy` t' = Nothing -- Term does not originate
          | otherwise = loop (pos + 1) c

-- At what position is a term acquired in a trace?
acquiredPos :: Algebra t p g s e c => t ->
               Trace t p g s e c -> Maybe Int
acquiredPos t c =
    loop 0 c
    where
      loop _ [] = Nothing       -- Term does not occur
      loop pos (In t' : c)
          | t `carriedBy` t' = Just pos -- Found it
          | t `occursIn` t' = Nothing   -- Occurs but is not carried
          | otherwise = loop (pos + 1) c
      loop pos (Out t' : c)
          | t `occursIn` t' = Nothing   -- Term occurs in outbound term
          | otherwise = loop (pos + 1) c

-- At what position do all of the variables in a term occur in a trace?
usedPos :: Algebra t p g s e c => t ->
           Trace t p g s e c -> Maybe Int
usedPos t c =
    loop 0 (varsInTerms [t]) c
    where
      loop _ _ [] = Nothing
      loop pos vars (e : c) =
          let vars' = [ x | x <- vars, notElem x (varsInTerms [evtTerm e]) ] in
          case vars' of
            [] -> Just pos
            _ -> loop (pos + 1) vars' c

-- Data flow analysis of a trace.

-- Return the minimal sets of parameters computed using traceFlow
flow :: Algebra t p g s e c => Trace t p g s e c -> [[t]]
flow c =
    toList $ filter minimal inits
    where
      inits = S.toList $ S.map fst $ traceFlow c (S.empty, S.empty)
      -- Is init minimal among sets in inits?
      minimal init = all (not . flip S.isProperSubsetOf init) inits
      -- Convert sets to lists and sort everything
      toList s = L.sort $ map (L.sort . S.toList) $ s

-- A flow rule maps an initial set of atoms and a set of available
-- terms to sets of pairs of the same sets.
type FlowRule t = (Set t, Set t) -> Set (Set t, Set t)

-- Analyze directed terms in a trace sequentially
traceFlow :: Algebra t p g s e c => Trace t p g s e c -> FlowRule t
traceFlow [] a = S.singleton a
traceFlow (d : c) a = comb (traceFlow c) (evtFlow d) a

-- Dispatch to algebra specific data flow routines
evtFlow :: Algebra t p g s e c => Event t p g s e c -> FlowRule t
evtFlow (In t) = inFlow t
evtFlow (Out t) = outFlow t

-- Combine flow rules sequentially
comb :: Algebra t p g s e c => FlowRule t -> FlowRule t -> FlowRule t
comb f g x =
    S.fold h S.empty (g x)
    where h a s = S.union (f a) s

data Role t p g s e c = Role
    { rname :: !String,
      rvars :: ![t],            -- Set of role variables
                                -- Events in causal order
      rtrace :: ![Event t p g s e c],
      -- Set of non-originating atoms, possibly with a trace length
      rnon :: ![(Maybe Int, t)], -- that says when to inherit the atom
      runique :: ![t],          -- Set of uniquely originating atoms
      rcomment :: [SExpr ()],   -- Comments from the input
      rnorig :: [(t, Int)],     -- Nons plus origination position
      ruorig :: [(t, Int)] }    -- Uniques plus origination position
    deriving Show

-- The empty role name is used with listener strands.  All roles in a
-- protocol must have a name with more than one character.

-- The lists vars, non, and unique are sets and should never contain
-- duplicate terms.

-- Create a role
mkRole :: Algebra t p g s e c => String -> [t] ->
          Trace t p g s e c -> [(Maybe Int, t)] -> [t] ->
          [SExpr ()] -> Role t p g s e c
mkRole name vars trace non unique comment =
    Role { rname = name,
           rvars = L.nub vars,  -- Every variable here must
           rtrace = trace,      --  occur in the trace.
           rnon = non,
           runique = L.nub unique,
           rcomment = comment,
           rnorig = map addNonOrig $ nonNub non,
           ruorig = map addUniqueOrig $ L.nub unique
         }
    where
      addUniqueOrig t =
          case originationPos t trace of
            Just p -> (t, p)
            Nothing -> error "Protocol.mkRole: Atom does not uniquely originate"
      addNonOrig (len, t) =
          case usedPos t trace of
            Nothing -> error "Protocol.mkRole: Atom variables not in trace"
            Just p ->
                case len of
                  Nothing -> (t, p)
                  Just len | len > p -> (t, len - 1)
                           | otherwise -> error msg
          where
            msg = "Protocol.mkRole: Length for atom too early in trace"
      -- Drop non-origination assumptions for the same atom.
      nonNub nons =
          reverse $ foldl f [] nons
          where
            f acc non@(_, t)
                | any (\(_, t') -> t == t') acc = acc
                | otherwise = non : acc

-- Protocols

data Prot t p g s e c
    = Prot { pname :: !String,  -- Name of the protocol
             alg :: !String,    -- Name of the algebra
             pgen :: !g,        -- Initial variable generator
             roles :: ![Role t p g s e c],
             pcomment :: [SExpr ()] }  -- Comments from the input
    deriving Show

-- Callers should ensure every role has a distinct name.
mkProt :: Algebra t p g s e c => String -> String ->
          g -> [Role t p g s e c] -> [SExpr ()] -> Prot t p g s e c
mkProt name alg gen roles comment =
    Prot { pname = name, alg = alg, pgen = gen,
           roles = roles, pcomment = comment }

