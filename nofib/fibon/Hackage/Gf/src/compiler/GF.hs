{-# OPTIONS -cpp #-}
module Main where

import GFC
import GFI
import GF.Data.ErrM
import GF.Infra.Option
import GF.Infra.UseIO
import Paths_gf

import Data.Version
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO
#ifdef mingw32_HOST_OS
import System.Win32.Console
import System.Win32.NLS
#endif

main :: IO ()
main = do
#ifdef mingw32_HOST_OS
  codepage <- getACP
  setConsoleCP codepage
  setConsoleOutputCP codepage
  enc <- mkTextEncoding ("CP"++show codepage)
  hSetEncoding stdin  enc
  hSetEncoding stdout enc
  hSetEncoding stderr enc
#endif
  args <- getArgs
  case parseOptions args of
    Ok (opts,files) -> do curr_dir <- getCurrentDirectory
                          lib_dir  <- getLibraryDirectory opts
                          mainOpts (fixRelativeLibPaths curr_dir lib_dir opts) files
    Bad err         -> do hPutStrLn stderr err
                          hPutStrLn stderr "You may want to try --help."
                          exitFailure

mainOpts :: Options -> [FilePath] -> IO ()
mainOpts opts files = 
    case flag optMode opts of
      ModeVersion     -> putStrLn $ "Grammatical Framework (GF) version " ++ showVersion version
      ModeHelp        -> putStrLn helpMessage
      ModeInteractive -> mainGFI opts files
      ModeRun         -> mainRunGFI opts files
      ModeCompiler    -> dieIOE (mainGFC opts files)

