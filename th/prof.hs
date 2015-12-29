{-# LANGUAGE TemplateHaskell #-}
import System.IO
import System.Environment
import Language.Haskell.Exts

main = do 
    [fn] <- getArgs
    fc <- readFile fn
    let ParseResult (Module _ _ _ _ _ _ decls) = parseModule fc
    print $ decls
