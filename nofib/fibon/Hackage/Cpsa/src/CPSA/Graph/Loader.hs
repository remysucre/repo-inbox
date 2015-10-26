-- Loads preskeletons and returns the results for display.  An error
-- is signaled if the ordering relation is cyclic.  For preskeleton
-- input without labels, a unique label is added.  The representation
-- of each strand includes its trace, but not its environment.
-- Ordering relations implied by transitive closure are eliminated so
-- as to reduce clutter.

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Graph.Loader (Preskel, Vertex, protocol, role, env, inst,
                          part, lastVertex, vertices, Node, vnode,
                          strands, label, parent, seen, unrealized,
                          protSrc, preskelSrc, initial, strand, pos,
                          prev, next, msg, succs, preds, loadDefs)
                          where

import qualified Data.List as L
import Control.Monad
import CPSA.Lib.CPSA

-- A view of protocols and preskeletons designed for display.

data Preskel = Preskel
    { protocol :: String,       -- Name of the protocol
      label :: Int,             -- Label from the input or generated
                                -- by the loader
      parent :: Maybe Int,      -- Parent from the input
      seen :: [Int], -- Seen preskeletons isomorphic to cohort members
      strands :: Int,           -- Number of strands
      initial :: [Vertex],      -- The initial node in each strand
      unrealized :: Maybe [Vertex], -- Nodes not realized if available
      protSrc :: SExpr Pos,       -- Source for the protocol
      preskelSrc :: SExpr Pos }   -- Source for this preskeleton
    deriving Show

instance Eq Preskel where
    k0 == k1 = label k0 == label k1

instance Ord Preskel where
    compare k0 k1 = compare (label k0) (label k1)

data Inst = Inst
    { part :: String,       -- Role name (empty if this is a listener)
      mapping :: String,  -- Environment (empty if this is a listener)
      trace :: [String] }      -- Transmit or reception directed terms
    deriving Show

-- A vertex v contains the information associated with the node
-- (strand v, pos v).
data Vertex = Vertex
    { msg :: String,
      inst :: Inst,
      prev :: Maybe Vertex,     -- Strand previous
      next :: Maybe Vertex,     -- Strand next
      preds :: [Vertex],        -- Cross strand predecessors
      succs :: [Vertex],        -- Cross strand successors
      strand :: Int,            -- Strand ID
      pos :: Int }              -- Position in strand

instance Eq Vertex where
    n0 == n1 = (strand n0, pos n0) == (strand n1, pos n1)

instance Ord Vertex where
    compare n0 n1 = compare (strand n0, pos n0) (strand n1, pos n1)

instance Show Vertex where
    showsPrec _ n = let (s, p) = (strand n, pos n) in
                    showChar '(' . shows s . showString ", " .
                    shows p . showChar ')'

-- Returns the role associated with the node
role :: Vertex -> String
role v = part (inst v)

-- Returns the env associated with the node
env :: Vertex -> String
env v = mapping (inst v)

lastVertex :: Vertex -> Vertex
lastVertex n =
    maybe n lastVertex (next n)

vertices :: Preskel -> [Vertex]
vertices k = foldl addNext [] (initial k)

addNext :: [Vertex] -> Vertex -> [Vertex]
addNext ns n = foldStrand (flip (:)) ns n

foldStrand :: (a -> Vertex -> a) -> a -> Vertex -> a
foldStrand f a n = maybe (f a n) (foldStrand f (f a n)) (next n)

-- The remaing is in support of the loader

-- The protocol data structure is used only within the loader

data Protocol = Protocol
    { name :: String,           -- Protocol name
      roles :: [(String, Trace)], -- A role is just a name and a trace
      src :: SExpr Pos }         -- Source of protocol
    deriving Show

type Trace = [SExpr Pos]        -- A list of directed messages

-- A node is a strand (an instance index), and a position along the
-- strand.
type Node = (Int, Int)

-- Convert a vertex in a diagram into a pair of natural numbers
vnode :: Vertex -> Node
vnode v = (strand v, pos v)

type Pair = (Node, Node)        -- An ordering of nodes

-- Load the initial comments and top-level defs or return an error
-- message

loadDefs :: Monad m => [SExpr Pos] -> m ([SExpr Pos], [Preskel])
loadDefs xs =
    do
      let (cmts, xs') = loadComments [] xs
      (_, ks, _) <- foldM loadSExpr ([], [], 0) xs'
      return (reverse cmts, reverse ks)

loadComments :: [SExpr Pos] -> [SExpr Pos] -> ([SExpr Pos], [SExpr Pos])
loadComments cmts (cmt@(L _ (S _ "comment" : _)): xs) = 
    loadComments (cmt:cmts) xs
loadComments cmts xs = (cmts, xs)

-- The integer is used to add a label to preskeleton without one.
type LoadSExpr = ([(String, Protocol)], [Preskel], Int)

loadSExpr :: Monad m => LoadSExpr -> SExpr Pos -> m LoadSExpr
loadSExpr (ps, ks, tag) x@(L pos (S _ "defprotocol" : xs)) =
    do
      p <- loadProt x pos xs
      return (p : ps, ks, tag)
loadSExpr (ps, ks, tag) x@(L pos (S _ "defskeleton" : xs)) =
    do
      k <- loadPreskel x pos ps tag xs
      -- Ensure labels are unique
      case any ((== (label k)) . label) ks of
        True -> fail (shows pos ("Duplicate label " ++ show (label k)))
        False -> return (ps, k : ks, tag + 1)
loadSExpr le (L _ (S _ "comment" : _)) = return le
loadSExpr _ x = fail (shows (annotation x) "Malformed input")

-- Protocols

loadProt :: Monad m => SExpr Pos -> Pos -> [SExpr Pos] ->
            m (String, Protocol)
loadProt x _ (S _ name : S _ _ : xs) =
    do
      roles <- loadRoles xs
      return (name, Protocol { name = name, roles = roles, src = x })
loadProt _ pos _ = fail (shows pos "Malformed protocol")

loadRoles :: Monad m => [SExpr Pos] -> m [(String, Trace)]
loadRoles (L pos (S _ "defrole" : x) : xs) =
    do
      r <- loadRole pos x
      rs <- loadRoles xs
      return (r : rs)
loadRoles _ = return []

loadRole :: Monad m => Pos -> [SExpr Pos] -> m (String, Trace)
loadRole _ (S _ name :
	    L _ (S _ "vars" : _) :
            L _ (S _ "trace" : trace) : _) = return (name, trace)
loadRole pos _ = fail (shows pos "Malformed role")

-- Preskeletons

loadPreskel :: Monad m => SExpr Pos -> Pos -> [(String, Protocol)] ->
               Int -> [SExpr Pos] -> m Preskel
loadPreskel s pos ps tag (S _ name : (L _ (S _ "vars" : _)) : xs) =
    case lookup name ps of
      Nothing -> fail (shows pos $ "Protocol " ++ name ++ " unknown")
      Just p ->
          do
            alist xs            -- Ensure alist syntax
            let cs = case assoc "traces" xs of
                       Nothing -> []
                       Just cs -> cs
            loadInsts s pos p tag [] cs xs
loadPreskel _ pos _ _ _ = fail (shows pos "Malformed skeleton")

loadInsts :: Monad m => SExpr Pos -> Pos -> Protocol -> Int ->
             [Inst] -> [SExpr Pos] -> [SExpr Pos] -> m Preskel
loadInsts s top p tag insts cs (L pos (S _ "defstrand" : x) : xs) =
    case x of
      S _ role : N _ height : env ->
          do
            let (c, cs') = case cs of
                      L _ c : cs' -> (c, cs')
                      _ -> ([], [])
            i <- loadInst pos p c role height env
            loadInsts s top p tag (i : insts) cs' xs
      _ ->
          fail (shows pos "Malformed defstrand")
loadInsts s top p tag insts cs (L pos (S _ "deflistener" : x) : xs) =
    case x of
      [term] ->
          let e0 = show (L pos [S pos "recv", term]) in
          let e1 = show (L pos [S pos "send", term]) in
          let i = Inst { part = "", mapping = "", trace = [e0, e1] } in
          let (_, cs') = case cs of
                           L _ c : cs' -> (c, cs')
                           _ -> ([], []) in
          loadInsts s top p tag (i : insts) cs' xs
      _ ->
          fail (shows pos "Malformed deflistener")
loadInsts s pos p tag revInsts _ xs =
    do
      label <- nassoc "label" xs
      parent <- nassoc "parent" xs
      seen <- nsassoc "seen" xs
      let insts = reverse revInsts
      case null insts of
        True -> fail (shows pos "No strands")
        False -> return ()
      let heights = map (length . trace) insts
      unrealized <- loadNodes heights (assoc "unrealized" xs)
      let orderings = maybe [] id (assoc "precedes" xs)
      pairs <- loadOrderings heights orderings
      let graph = adj heights pairs
      let pairs' = reduce graph pairs
      let strands = length insts
      let nodes = [(s, p) | s <- nats strands,
                            p <- nats $ heights !! s]
      case isAcyclic graph nodes of
        False -> fail (shows pos "Cycle found")
        True ->
            return Preskel { protocol = name p,
                             label = maybe tag id label,
                             parent  = parent,
                             seen = maybe [] (L.sort . L.nub) seen,
                             strands = strands,
                             initial = initial,
                             unrealized = unrealized',
                             protSrc = src p,
                             preskelSrc = s }
            where
              initial = map head nodes -- The first node in each strand
              nodes = [ [ Vertex {
                            msg = msg,
                            inst = inst,
                            prev = getPrev (s, p),
                            next = getNext ht (s, p),
                            preds = getPreds (s, p),
                            succs = getSuccs (s, p),
                            strand = s,
                            pos = p } |
                          (p, msg) <- zip [0..] (trace inst) ] |
                        (s, inst) <- zip [0..] insts,
                        let ht = length (trace inst) ]
              getNode (s, p) = nodes !! s !! p
              getPrev (s, p)
                  | p > 0 = Just (getNode (s, p - 1))
                  | otherwise = Nothing
              getNext ht (s, p)
                  | p + 1 < ht= Just (getNode (s, p + 1))
                  | otherwise = Nothing
              getPreds n = [ getNode n0 | (n0, n1) <- pairs', n == n1 ]
              getSuccs n = [ getNode n1 | (n0, n1) <- pairs', n == n0 ]
              unrealized' =
                  maybe Nothing (Just . map getNode) unrealized

-- Construct an adjacency representation of the ordering relation.
-- The first argument is a list giving the length of each strand.
adj :: [Int] -> [Pair] -> Node -> [Node]
adj strands orderings (s, p) =
    [ strand s h | (s, h) <- zip [0..] strands ] !! s !! p
    where
      strand s h = [ entry (s, p) | p <- nats h ]
      entry n = enrich n [ n0 | (n0, n1) <- orderings, n1 == n ]
      -- add strand succession edges
      enrich (s, p) ns
          | p > 0 = (s, p - 1) : ns
          | otherwise = ns

-- Transitive Reduction

-- An edge is essential if its removal eliminates all paths from its
-- source to its destination.  This function removes all non-essential
-- edges from the ordering relation.
reduce :: (Node -> [Node]) -> [Pair] -> [Pair]
reduce graph orderings =
    filter essential orderings
    where
      essential (src, dst) =
          loop dst (L.delete dst (graph src)) [src]
      loop _ [] _ = True        -- No other path found
      loop dst (n : ns) seen
          | n == dst = False    -- There is another path
          | elem n seen = loop dst ns seen
          | otherwise = loop dst (graph n ++ ns) (n : seen)

-- Instances

loadInst :: Monad m => Pos -> Protocol -> [SExpr Pos] -> String ->
            Int -> [SExpr Pos] -> m Inst
loadInst pos p c role ht env =
    case lookup role (roles p) of
      Nothing ->
          fail (shows pos $ "Role " ++ role ++ " not found in " ++ name p)
      Just trace ->
          case ht < 1 || ht > length trace of
            True -> fail (shows pos "Bad height")
            False ->
                do
                  let mapping = show (L pos env)
                  env <- loadMaplet env
                  let evts = itrace env c (take ht trace)
                  return Inst { part = role,
                                mapping = mapping,
                                trace = evts }

-- Compute an instance's trace, using the traces output where possible.
itrace :: [(String, SExpr Pos)] -> [SExpr Pos] -> [SExpr Pos] -> [String]
itrace _ _ [] = []
itrace env (c : cs) (_: rs) =
    show c : itrace env cs rs
itrace env [] (r : rs) =
    show (subst env r) : itrace env [] rs

loadMaplet :: Monad m => [SExpr Pos] -> m [(String, SExpr Pos)]
loadMaplet (L _ [S _ name, x] : xs) =
    do
      env <- loadMaplet xs
      return ((name, x) : env)
loadMaplet [] = return []
loadMaplet (x:_) = fail (shows (annotation x) "Bad maplet")

subst :: [(String, SExpr Pos)] -> SExpr Pos -> SExpr Pos
subst env x@(S _ name) =
    case lookup name env of
      Nothing -> x
      Just x' -> x'
subst env (L pos (x : xs)) = L pos (x : map (subst env) xs)
subst _ x = x

-- Ensure alist has the proper form
alist :: Monad m => [SExpr Pos] -> m ()
alist [] = return ()
alist ((L _ (S _ _ : _)) : xs) = alist xs
alist xs = fail (shows (annotation $ head xs) "Malformed association list")

-- Lookup value in alist, appending values with the same key
assoc :: String -> [SExpr Pos] -> Maybe [SExpr Pos]
assoc key alist =
    loop alist Nothing
    where
      loop ((L _ (S _ head : tail)) : rest) vals
          | key == head = loop rest (extend tail vals)
          | otherwise = loop rest vals
      loop _ vals = vals
      extend x Nothing = Just x
      extend x (Just y) = Just (x ++ y)

-- assoc key alist =
--    concat [ rest | L _ (S _ head : rest) <- alist, key == head ]

nassoc :: Monad m => String -> [SExpr Pos] -> m (Maybe Int)
nassoc key xs =
    case assoc key xs of
      Nothing -> return Nothing
      Just [val] ->
          do
            ns <- num val
            return (Just ns)
      Just (x:_) -> fail (shows (annotation x) "Expecting one number")
      Just [] -> fail (shows (annotation (head xs)) "Expecting one number")

num :: Monad m => SExpr Pos -> m Int
num (N _ n) = return n
num x = fail (shows (annotation x) "Expecting a number")

nsassoc :: Monad m => String -> [SExpr Pos] -> m (Maybe [Int])
nsassoc key xs =
    case assoc key xs of
      Nothing -> return Nothing
      Just val ->
          do
            ns <- mapM num val
            return (Just ns)

loadOrderings :: Monad m => [Int] -> [SExpr Pos] -> m [Pair]
loadOrderings heights x =
    foldM f [] x
    where
      f ns x =
          do
            np <- loadPair heights x
            return (adjoin np ns)

loadPair :: Monad m => [Int] -> SExpr Pos -> m Pair
loadPair heights (L pos [x0, x1]) =
    do
      n0 <- loadNode heights x0
      n1 <- loadNode heights x1
      case sameStrands n0 n1 of  -- Same strand
        True -> fail (shows pos "Malformed pair -- nodes in same strand")
        False -> return (n0, n1)
    where
      sameStrands (s0, _) (s1, _) = s0 == s1
loadPair _ x = fail (shows (annotation x) "Malformed pair")

loadNode :: Monad m => [Int] -> SExpr Pos -> m Node
loadNode heights (L pos [N _ s, N _ p])
    | s < 0 = fail (shows pos "Negative strand in node")
    | p < 0 = fail (shows pos "Negative position in node")
    | otherwise =
        case height heights s of
          Nothing -> fail (shows pos "Bad strand in node")
          Just h | p < h -> return (s, p)
          _ -> fail (shows pos "Bad position in node")
    where
      height [] _ = Nothing
      height (x: xs) s          -- Assume s non-negative
          | s == 0 = Just x
          | otherwise = height xs (s - 1)
loadNode _ x = fail (shows (annotation x) "Malformed node")

loadNodes :: Monad m => [Int] -> Maybe [SExpr Pos] -> m (Maybe [Node])
loadNodes _ Nothing = return Nothing
loadNodes heights (Just xs) =
    do
      ns <- mapM (loadNode heights) xs
      return (Just ns)
