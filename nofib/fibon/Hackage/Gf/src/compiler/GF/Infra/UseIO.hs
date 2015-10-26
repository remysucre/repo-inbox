{-# OPTIONS -cpp #-}
----------------------------------------------------------------------
-- |
-- Module      : UseIO
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/08/08 09:01:25 $ 
-- > CVS $Author: peb $
-- > CVS $Revision: 1.17 $
--
-- (Description of the module)
-----------------------------------------------------------------------------

module GF.Infra.UseIO where

import GF.Data.Operations
import GF.Infra.Option
import Paths_gf(getDataDir)

import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Environment
import System.Exit
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Exception(evaluate)
import qualified Data.ByteString.Char8 as BS
import Data.List(nub)

putShow' :: Show a => (c -> a) -> c -> IO ()
putShow' f = putStrLn . show . length . show . f

putIfVerb :: Options -> String -> IO ()
putIfVerb opts msg =
      when (verbAtLeast opts Verbose) $ putStrLn msg

putIfVerbW :: Options -> String -> IO ()
putIfVerbW opts msg =
      when (verbAtLeast opts Verbose) $ putStr (' ' : msg)

errOptIO :: Options -> a -> Err a -> IO a
errOptIO os e m = case m of
  Ok x  -> return x
  Bad k -> do  
    putIfVerb os k
    return e

type FileName = String
type InitPath = String
type FullPath = String

gfLibraryPath    = "GF_LIB_PATH"
gfGrammarPathVar = "GF_GRAMMAR_PATH"

getLibraryDirectory :: Options -> IO FilePath
getLibraryDirectory opts =
  case flag optGFLibPath opts of
    Just path -> return path
    Nothing   -> catch
                   (getEnv gfLibraryPath)
                   (\ex -> getDataDir >>= \path -> return (path </> "lib"))

getGrammarPath :: FilePath -> IO [FilePath]
getGrammarPath lib_dir = do
  catch (fmap splitSearchPath $ getEnv gfGrammarPathVar) (\_ -> return [lib_dir </> "prelude"])     -- e.g. GF_GRAMMAR_PATH

-- | extends the search path with the
-- 'gfLibraryPath' and 'gfGrammarPathVar'
-- environment variables. Returns only existing paths.
extendPathEnv :: Options -> IO [FilePath]
extendPathEnv opts = do
  opt_path <- return $ flag optLibraryPath opts         -- e.g. paths given as options
  lib_dir  <- getLibraryDirectory opts                  -- e.g. GF_LIB_PATH
  grm_path <- getGrammarPath lib_dir                   -- e.g. GF_GRAMMAR_PATH
  let paths = opt_path ++ [lib_dir] ++ grm_path
  ps <- liftM concat $ mapM allSubdirs paths
  mapM canonicalizePath ps
  where
    allSubdirs :: FilePath -> IO [FilePath]
    allSubdirs [] = return [[]]
    allSubdirs p = case last p of
      '*' -> do let path = init p
                fs <- getSubdirs path
                return [path </> f | f <- fs]
      _   -> do exists <- doesDirectoryExist p
                if exists
                  then return [p]
                  else return []

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs dir = do
  fs  <- catch (getDirectoryContents dir) (const $ return [])
  foldM (\fs f -> do let fpath = dir </> f
                     p <- getPermissions fpath
                     if searchable p && not (take 1 f==".")
                       then return (fpath:fs)
                       else return        fs ) [] fs

justModuleName :: FilePath -> String
justModuleName = dropExtension . takeFileName

splitInModuleSearchPath :: String -> [FilePath]
splitInModuleSearchPath s = case break isPathSep s of
  (f,_:cs) -> f : splitInModuleSearchPath cs
  (f,_)    -> [f]
  where
    isPathSep :: Char -> Bool
    isPathSep c = c == ':' || c == ';'

--

putStrFlush :: String -> IO ()
putStrFlush s = putStr s >> hFlush stdout

putStrLnFlush :: String -> IO ()
putStrLnFlush s = putStrLn s >> hFlush stdout

-- * IO monad with error; adapted from state monad

newtype IOE a = IOE (IO (Err a))

appIOE :: IOE a -> IO (Err a)
appIOE (IOE iea) = iea

ioe :: IO (Err a) -> IOE a
ioe = IOE

ioeIO :: IO a -> IOE a
ioeIO io = ioe (io >>= return . return)

ioeErr :: Err a -> IOE a
ioeErr = ioe . return 

instance  Monad IOE where
  return a    = ioe (return (return a))
  IOE c >>= f = IOE $ do 
                  x <- c          -- Err a
                  appIOE $ err ioeBad f x         -- f :: a -> IOE a

ioeBad :: String -> IOE a
ioeBad = ioe . return . Bad

useIOE :: a -> IOE a -> IO a
useIOE a ioe = appIOE ioe >>= err (\s -> putStrLn s >> return a) return 

foldIOE :: (a -> b -> IOE a) -> a -> [b] -> IOE (a, Maybe String)
foldIOE f s xs = case xs of
  [] -> return (s,Nothing)
  x:xx -> do
    ev <- ioeIO $ appIOE (f s x) 
    case ev of 
      Ok v  -> foldIOE f v xx
      Bad m -> return $ (s, Just m)

dieIOE :: IOE a -> IO a
dieIOE x = appIOE x >>= err die return

die :: String -> IO a
die s = do hPutStrLn stderr s
           exitFailure

putStrLnE :: String -> IOE ()
putStrLnE = ioeIO . putStrLnFlush 

putStrE :: String -> IOE ()
putStrE = ioeIO . putStrFlush 

putPointE :: Verbosity -> Options -> String -> IOE a -> IOE a
putPointE v opts msg act = do
  when (verbAtLeast opts v) $ ioeIO $ putStrFlush msg

  t1 <- ioeIO $ getCPUTime
  a <- act >>= ioeIO . evaluate
  t2 <- ioeIO $ getCPUTime

  if flag optShowCPUTime opts
      then do let msec = (t2 - t1) `div` 1000000000
              putStrLnE (printf " %5d msec" msec)
      else when (verbAtLeast opts v) $ putStrLnE ""

  return a
