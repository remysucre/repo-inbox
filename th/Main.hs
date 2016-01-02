{-# LANGUAGE TemplateHaskell #-}
import System.IO
import System.Environment
import Language.Haskell.TH
import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import Language.Haskell.Meta.Syntax.Translate
import Splices

-- this program tries to load functions from a Haskell source file, 
-- do some transformations to the functions, splice them back in 
-- and call them. 
-- what I have now will parse a source file into TH types and print
-- out the first declaration.

main = do 
    [fn] <- getArgs
    fc <- readFile fn
    let ParseOk (Module _ _ _ _ _ _ decls) = parseModule fc
        decl = head decls
        dec = toDec decl
    $(getLet dec)
    -- print dec
    -- print $(LetE [dec] [|try 1|])
