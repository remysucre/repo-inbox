-- Expands macros using definitions in the input

-- Copyright (c) 2009 The MITRE Corporation
--
-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the BSD License as published by the
-- University of California.

module CPSA.Lib.Expand (expand) where

import Control.Monad
import CPSA.Lib.CPSA

-- The macroexpand loop limit
limit :: Int
limit = 1000

expand :: Monad m => [SExpr Pos] -> m [SExpr Pos]
expand sexprs =
    do
      (_, sexprs) <- foldM expandSExpr ([], []) sexprs
      return (reverse sexprs)

expandSExpr :: Monad m => ([Macro], [SExpr Pos]) -> SExpr Pos ->
               m ([Macro], [SExpr Pos])
expandSExpr (macs, sexprs) (L pos (S _ "defmacro" : xs)) =
    do                          -- Process a macro definition
      mac <- defmacro pos xs
      return (mac : macs, sexprs)
expandSExpr (macs, sexprs) sexpr =
    do                          -- Process all other S-expressions
      sexpr <- expandAll macs sexpr
      return (macs, sexpr : sexprs)

-- A macro definition is of the form:
--
-- (defmacro (NAME ARG*) BODY)
--
-- where NAME and each ARG is a symbol

data Macro = Macro
    { name :: String,
      args :: [String],
      body :: SExpr Pos }

defmacro :: Monad m => Pos -> [SExpr Pos] -> m Macro
defmacro _ [L _ (name : args), body] =
    do
      name <- symbol name
      args <- mapM symbol args
      return $ Macro { name = name,
                       args = args,
                       body = body}
defmacro pos _ = fail (shows pos "Malformed macro")

symbol :: Monad m => SExpr Pos -> m String
symbol (S _ string) = return string
symbol x = fail (shows (annotation x) "Expecting a symbol")

-- Expand an S-expression using a given set of macros

expandAll :: Monad m => [Macro] -> SExpr Pos -> m (SExpr Pos)
expandAll macs sexpr =
    do
      sexpr <- macroExpand macs (annotation sexpr) limit sexpr
      case sexpr of
        L pos xs ->             -- Expand elements of list
            do
              xs <- mapM (expandAll macs) xs
              return (L pos xs)
        _ -> return sexpr

-- Expand one S-expression limiting the number of expansions.

macroExpand :: Monad m => [Macro] -> Pos ->  Int ->
               SExpr Pos -> m (SExpr Pos)
macroExpand _ pos limit _
    | limit <= 0 = fail (shows pos "Expansion limit exceded")
macroExpand macs pos limit sexpr@(L _ (S _ sym : xs)) =
    case macroExpand1 macs sym xs of
      Nothing -> return sexpr   -- Nothing to do
      Just sexpr -> macroExpand macs pos (limit - 1) sexpr
macroExpand _ _ _ sexpr = return sexpr

-- Expand one macro call or return Nothing

macroExpand1 :: [Macro] -> String -> [SExpr Pos] -> Maybe (SExpr Pos)
macroExpand1 [] _ _ = Nothing
macroExpand1 (mac : macs) sym xs
    | name mac == sym && length (args mac) == length xs =
        Just (apply mac xs)
    | otherwise =
        macroExpand1 macs sym xs

-- Apply a macro to some parameters

apply :: Macro -> [SExpr Pos] -> SExpr Pos
apply mac xs =
    subst (zip (args mac) xs) (body mac)

-- Substitute parameters into the macro body

subst :: [(String, SExpr Pos)] -> SExpr Pos -> SExpr Pos
subst env sexpr@(S _ sym) =
    maybe sexpr id (lookup sym env)
subst env (L pos sexprs) =
    L pos (map (subst env) sexprs)
subst _ sexpr = sexpr
