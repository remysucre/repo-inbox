{-# LANGUAGE BangPatterns#-}

import System.IO
import System.Process
import GHC.Stats

main = do
    system "./Main +RTS -ttiming.temp --machine-readable"
    f <- readFile "timing.temp"
    system "rm timing.temp"
    putStr f
